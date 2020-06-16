{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad (join)
import           Data.Coerce (coerce)
import           Foreign.Matlab
import           Foreign.Matlab.Engine
import           Foreign.Matlab.Engine.Wrappers
import           COBRA
import           Data.Map.Lens
import qualified Data.Map.Strict as DM
import           Path
import           ZIO.Trans

data Env = Env {mEngine :: Engine}
  deriving Eq

zslift :: IO a -> ZIO r String a
zslift = (mapZError show) . zlift

main :: IO ()
main = do
  eng <- newEngine ""
  let env = Env eng
  runApp app env
  where
    runApp a r = runZIO a r putStrLn

app :: ZIO Env String ()
app = do
  env <- ask
  let eng = mEngine env
  zslift $ diaryFile eng logFile
  zslift $ diaryOn eng
  pl <- zslift $ permListMX 5

  zslift $ runAll $ (disp eng) <$> pl

  -- Pure Haskell version:
  -- let pl = permList 5
  -- runAll $ (putStrLn . show) <$> pl

  zslift $ initHSMatlabEngineEnv eng [initDMM, initCobraToolbox]
  pure ()

logFile :: Path Rel File
logFile = $(mkRelFile "log_prioSims.txt")

initDMM :: Engine -> IO ()
initDMM eng = do
  engineEvalProc eng "initDMM" []

newtype MultiModel = MultiModel { _multiModel :: MStruct }
makeLenses '' MultiModel

newtype InfoCom = InfoCom { _infoCom :: MStruct }
makeLenses '' InfoCom

getInfoCom :: MultiModel -> MIO (Either String InfoCom)
getInfoCom model = do
  let infoComAA = model ^. multiModel . mStruct . at "infoCom" & errMsgAt
  infoComSA <- infoComAA & traverse castMXArray <&> sequence <&> errMsg <&> join
  infoComFirst <- infoComSA & traverse mxArrayGetFirst <&> join
  pure $ InfoCom <$> infoComFirst
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getInfoCom: couldn't find field"
    errMsg = mayToEi "getInfoCom: couldn't cast"

newtype SpeciesAbbr = SpeciesAbbr { _speciesAbbr :: String }
makeLenses '' SpeciesAbbr

newtype StepResult = StepResult { _stepResult :: MStruct }
makeLenses '' StepResult

newtype ScheduleResult = ScheduleResult { _scheduleResult :: MXArray MCell }
makeLenses '' ScheduleResult

newtype SteadyComOpts = SteadyComOpts { _steadyComOpts :: MStruct }
makeLenses '' SteadyComOpts

type ModelMap = DM.Map SpeciesAbbr MultiModel

-- | Wraps a struct containing essentiality information
newtype EssInfo = EssInfo { _essInfo :: MStruct }
makeLenses '' EssInfo

semiDynamicSteadyCom :: Engine
  -> ModelMap
  -- ^ List of multi-species models.
  -> [SpeciesAbbr]
  -> ScheduleResult
  -> Maybe SteadyComOpts
  -> VarArgIn
  -> IO ScheduleResult

-- Base case
semiDynamicSteadyCom (eng :: Engine)
  (_        :: ModelMap)
  ([]       :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (_        :: Maybe SteadyComOpts)
  (_        :: VarArgIn)
    = pure schedRes

-- Inductive case
semiDynamicSteadyCom (eng :: Engine)
  (modelMap :: ModelMap)
  (currentSched:schedRemain :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (optsOverride :: Maybe SteadyComOpts)
  (varargin :: VarArgIn) = do
    arrays <- schedRes ^. scheduleResult & mxCellGetArraysOfType
    lastResEi :: Either String MStruct <- (mapLast mxArrayGetFirst arrays)
      & sequence <&> errMsgSchedResEmpty <&> join
    pure schedRes -- TODO : change this to actual result
  where
    errMsgSchedResEmpty = mayToEi "semiDynamicSteadyCom: empty schedRes cell array"

-- | Helper function to determine how bounds are changed based on
-- | prior model state.
semiDynamicSteadyComUpdateBounds :: Engine
  -> MultiModel
  -> MultiModel
  -> MXArray MDouble
  -> [EssInfo]
  -> IO (Either String (MXArray MDouble))
semiDynamicSteadyComUpdateBounds (eng :: Engine)
  (model :: MultiModel)
  (modelPrior :: MultiModel)
  (fluxPrior :: MXArray MDouble)
  (essInfo :: [EssInfo]) = do
  mxEssInfo <- fromListIO $ (coerce essInfo :: [MStruct])
  [res] <- engineEvalFun eng "semiDynamicSteadyComUpdateBounds" [
      EvalStruct $ model ^. multiModel
    , EvalStruct $ modelPrior ^. multiModel
    , EvalArray $ anyMXArray fluxPrior
    , EvalArray $ anyMXArray mxEssInfo
    ] 1
  errMsg <$> castMXArray res
  where
    errMsg = mayToEi "semiDynamicSteadyComUpdateBounds: couldn't cast"


semiDynamicSteadyComStep :: Engine
  -> MultiModel
  -> [SpeciesAbbr]
  -> SteadyComOpts
  -> [EssInfo]
  -> VarArgIn
  -> IO (Either String StepResult)
semiDynamicSteadyComStep (eng :: Engine)
  (modelCom :: MultiModel)
  (currentSched :: [SpeciesAbbr])
  (optsOverride :: SteadyComOpts)
  (essentialRxns :: [EssInfo])
  (varargin :: VarArgIn) = do
  mxEssInfo <- fromListIO $ (coerce essentialRxns :: [MStruct])
  mxCurrentSched <- mxSchedule currentSched
  let mxVarargin = mxVarArgs varargin
  allArgs <- pure $ [
      EvalStruct $ modelCom ^. multiModel
    , EvalArray $ anyMXArray mxCurrentSched
    , EvalStruct $ optsOverride ^. steadyComOpts
    , EvalArray $ anyMXArray mxEssInfo
    ] ++ mxVarargin
  [res] <- engineEvalFun eng "semiDynamicSteadyComStep" allArgs 1
  resArrEi <- errMsg <$> castMXArray res
  resStructEi <- join <$> (sequence $ mxArrayGetFirst <$> resArrEi)
  pure $ StepResult <$> resStructEi
  where
    errMsg = mayToEi "semiDynamicSteadyComStep: couldn't cast"


mxSchedule :: [SpeciesAbbr] -> IO (MXArray MCell)
mxSchedule sched = cellFromListsIO $ (coerce sched :: [String])

checkEssentiality :: Engine -> MultiModel -> [String] -> IO (Either String [EssInfo])
checkEssentiality eng model rxns = do
  rxnsCA <- cellFromListsIO rxns
  [res] <- engineEvalFun eng "checkEssentiality" [
      EvalStruct $ model ^. multiModel
    , EvalArray $ anyMXArray rxnsCA] 1
  essArrMay :: Maybe MStructArray <- castMXArray res
  listOfStructsMay <- sequence $ mxArrayGetAll <$> essArrMay
  pure $ (fmap . fmap) EssInfo (mayToEi "checkEssentiality: couldn't cast" listOfStructsMay)


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
import qualified Data.Map.Strict as DM
import           Path

main :: IO ()
main = do
  eng <- newEngine ""
  diaryFile eng logFile
  diaryOn eng
  pl <- permListMX 5

  runAll $ (disp eng) <$> pl

  -- Pure Haskell version:
  -- let pl = permList 5
  -- runAll $ (putStrLn . show) <$> pl

  initHSMatlabEngineEnv eng [initDMM, initCobraToolbox]
  pure ()

logFile :: Path Rel File
logFile = $(mkRelFile "log_prioSims.txt")

initDMM :: Engine -> IO ()
initDMM eng = do
  engineEvalProc eng "initDMM" []

newtype MultiModel = MultiModel { _multiModel :: MStruct }
makeLenses '' MultiModel

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
  -> ScheduleResult

--Base case
semiDynamicSteadyCom (eng :: Engine)
  (modelMap :: ModelMap)
  ([] :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (optsOverride :: Maybe SteadyComOpts)
  (varargin :: VarArgIn)
    = schedRes

-- semiDynamicSteadyCom (eng :: Engine)
--   (modelMap :: ModelMap)
--   (currentSched:schedRemain :: [SpeciesAbbr])
--   (schedRes :: ScheduleResult)
--   (optsOverride :: Maybe SteadyComOpts)
--   (varargin :: VarArgIn) = do
    
    

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

mayToEi :: e -> Maybe a -> Either e a
mayToEi err = maybe (Left err) Right

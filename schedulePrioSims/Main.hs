{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

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

newtype MultiModel = MultiModel { unMultiModel :: MStruct }

newtype SpeciesAbbr = SpeciesAbbr { unSpeciesAbbr :: String }

newtype StepResult = StepResult { unStepResult :: MStruct }

newtype ScheduleResult = ScheduleResult { unScheduleResult :: MXArray MCell }

newtype SteadyComOpts = SteadyComOpts { unSteadyComOpts :: MStruct }

type ModelMap = DM.Map SpeciesAbbr MultiModel

-- | Wraps a struct containing essentiality information
newtype EssInfo = EssInfo { unEssInfo :: MStruct }

semiDynamicSteadyCom :: Engine
  -> ModelMap
  -- ^ List of multi-species models.
  -> [SpeciesAbbr]
  -> ScheduleResult
  -> Maybe SteadyComOpts
  -> VarArgIn
  -> ScheduleResult

semiDynamicSteadyCom (eng :: Engine)
  (modelMap :: ModelMap)
  (schedule :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (optsOverride :: Maybe SteadyComOpts)
  (varargin :: VarArgIn) =
  schedRes -- TODO

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
      EvalStruct $ unMultiModel model
    , EvalStruct $ unMultiModel modelPrior
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
      EvalStruct $ unMultiModel modelCom
    , EvalArray $ anyMXArray mxCurrentSched
    , EvalStruct $ unSteadyComOpts optsOverride
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
      EvalStruct $ unMultiModel model
    , EvalArray $ anyMXArray rxnsCA] 1
  essArrMay :: Maybe MStructArray <- castMXArray res
  listOfStructsMay <- sequence $ mxArrayGetAll <$> essArrMay
  pure $ (fmap . fmap) EssInfo (mayToEi "checkEssentiality: couldn't cast" listOfStructsMay)

mayToEi :: e -> Maybe a -> Either e a
mayToEi err = maybe (Left err) Right

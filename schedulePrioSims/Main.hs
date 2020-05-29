{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Foreign.Matlab
import Foreign.Matlab.Engine
import COBRA
import qualified Data.Map.Strict as DM
import Path

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

type VarArgIn = DM.Map String MAnyArray

newtype MultiModel = MultiModel { unMultiModel :: MStructArray }

newtype SpeciesAbbr = SpeciesAbbr { unSpeciesAbbr :: String }

newtype ScheduleResult = ScheduleResult { unScheduleResult :: MXArray MCell }

newtype SteadyComOpts = SteadyComOpts { unSteadyComOpts :: MStructArray }

type ModelMap = DM.Map SpeciesAbbr MultiModel

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

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Foreign.Matlab.Engine
import COBRA
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

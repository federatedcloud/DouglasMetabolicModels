{-# LANGUAGE TemplateHaskell #-}

module Main where

import Foreign.Matlab.Engine
import Lib
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
  pure ()

logFile :: Path Rel File
logFile = $(mkRelFile "log_prioSims.txt")

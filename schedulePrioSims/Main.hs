module Main where

import Foreign.Matlab.Engine
import Lib

main :: IO ()
main = do
  eng <- newEngine ""
  pl <- permListMX 5

  runAll $ (disp eng) <$> pl

  -- Pure Haskell version:
  -- let pl = permList 5
  -- runAll $ (putStrLn . show) <$> pl
  pure ()

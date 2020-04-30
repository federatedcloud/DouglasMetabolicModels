module Main where

import Lib

main :: IO ()
main = do
  let pl = permList 5
  runAll $ (putStrLn . show) <$> pl
  pure ()

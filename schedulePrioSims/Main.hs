module Main where

import Data.List (permutations)
import Lib

-- Note that if `f` is IO, the effect is not run.
absolveUnits :: f () -> ()
absolveUnits _ = ()


runIOs :: [IO ()] -> IO ()
runIOs as = absolveUnits <$> sequence as

permList :: Integer -> [[Integer]]
permList n = permutations [1..n]

main :: IO ()
main = do
  let pl = permList 5
  runIOs $ (putStrLn . show) <$> pl
  pure ()

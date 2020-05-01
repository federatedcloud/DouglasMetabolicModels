module COBRA.Analysis where

import Data.List (permutations)
import Control.Lens
import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)

permList :: Int -> [[Int]]
permList n = permutations [1..n]




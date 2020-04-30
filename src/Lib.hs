{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Foldable (toList)
import Data.List (permutations)
import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)
import Path

-- | Functions of this type take in a Haskell value and returns an array of doubles from MATLAB
type MArrayFun a = forall b. MType b b => a -> IO (MXArray b)


-- | Note that if `f` is IO, the effect is not run,
-- | as it is not necessary in a lazy language.
absolveUnits :: f () -> ()
absolveUnits _ = ()


runAll :: Monad t => [t ()] -> t ()
runAll as = absolveUnits <$> sequence as

permList :: Integer -> [[Integer]]
permList n = permutations [1..n]


travToMXArray :: (Traversable t, MXArrayComponent a) => t a -> MIO (MXArray a)
travToMXArray xs = do
  mxarr <- createMXArray [1, length xs]
  mxArraySetAll mxarr (toList xs) 
  pure mxarr

{-# LANGUAGE RankNTypes #-}

module COBRA.MATLAB.Util where

import Data.Foldable (toList)
import Data.List (permutations)
import Control.Lens
import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)
import Path


-- | Functions of this type take in a Haskell value and returns an array of doubles from MATLAB
type MArrayFun a = forall a. MXArrayComponent a => a -> IO (MXArray a)


travToMXArray :: (Traversable t, MXArrayComponent a) => t a -> MIO (MXArray a)
travToMXArray xs = do
  mxarr <- createMXArray [1, length xs]
  mxArraySetAll mxarr (toList xs) 
  pure mxarr


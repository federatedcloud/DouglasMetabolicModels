{-# LANGUAGE RankNTypes #-}

module COBRA.MATLAB.Util where

import           Data.Foldable (toList)
import           Data.List (permutations)
import           Control.Lens
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans


-- | Functions of this type take in a Haskell value and returns an array of doubles from MATLAB
type MArrayFun a = forall r e a. MXArrayComponent a => a -> ZIO r e (MXArray a)


travToMXArray :: (Traversable t, MXArrayComponent a) => t a -> ZIO r MatlabException (MXArray a)
travToMXArray xs = do
  mxarr <- createMXArray [1, length xs]
  mxArraySetAll mxarr (toList xs) 
  pure mxarr


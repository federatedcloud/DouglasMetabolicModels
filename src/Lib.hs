{-# LANGUAGE RankNTypes #-}

module Lib where

import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)
import Path

-- | Functions of this type take in a Haskell value and returns an array of doubles from MATLAB
type MArrayFun a = forall b. MType b b => a -> IO (MXArray b)



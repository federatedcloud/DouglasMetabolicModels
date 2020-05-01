{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Foldable (toList)
import Data.List (permutations)
import Control.Lens
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

discard :: a -> ()
discard _ = ()

runAll :: Monad t => [t ()] -> t ()
runAll as = absolveUnits <$> sequence as

permList :: Int -> [[Int]]
permList n = permutations [1..n]

permListMX :: Int -> MIO [MXArray MDouble]
permListMX n = permList n ^.. folded . to (fmap fromIntegral) <&> travToMXArray & sequence

travToMXArray :: (Traversable t, MXArrayComponent a) => t a -> MIO (MXArray a)
travToMXArray xs = do
  mxarr <- createMXArray [1, length xs]
  mxArraySetAll mxarr (toList xs) 
  pure mxarr

-- | Wrapper for `disp` in MATLAB; mostuly for debugging/info
disp :: MXArrayComponent a => Engine -> MXArray a -> MIO ()
disp eng arr = engineEvalFun eng "disp" [EvalArray arr] 0 <&> discard



diaryFile :: Engine -> Path b File -> MIO ()
diaryFile e logFile = engineEval e $ "diary " <> (toFilePath logFile)

diaryOn :: Engine -> MIO ()
diaryOn e = engineEval e "diary on"

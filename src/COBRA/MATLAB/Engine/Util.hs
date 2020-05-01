module COBRA.MATLAB.Engine.Util where

import COBRA.Syntax
import Data.Functor ((<&>))
import Foreign.Matlab
import Foreign.Matlab.Array
import Foreign.Matlab.Engine
import Foreign.Matlab.Engine.Wrappers (addpath)
import Path

-- | Wrapper for `disp` in MATLAB; mostuly for debugging/info
disp :: MXArrayComponent a => Engine -> MXArray a -> MIO ()
disp eng arr = engineEvalFun eng "disp" [EvalArray arr] 0 <&> discard

diaryFile :: Engine -> Path b File -> MIO ()
diaryFile e logFile = engineEval e $ "diary " <> (toFilePath logFile)

diaryOn :: Engine -> MIO ()
diaryOn e = engineEval e "diary on"

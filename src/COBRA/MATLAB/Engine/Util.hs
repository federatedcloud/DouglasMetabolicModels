module COBRA.MATLAB.Engine.Util where

import           COBRA.Syntax
import           Data.Functor ((<&>))
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOEngine.Wrappers (addpath)
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans

-- | Wrapper for `disp` in MATLAB; mostuly for debugging/info
disp :: (HasEngine r, MXArrayComponent a) => MXArray a -> ZIO r MatlabException ()
disp arr = engineEvalFun "disp" [EvalArray arr] 0 <&> discard

diaryFile :: HasEngine r => Path b File -> ZIO r MatlabException ()
diaryFile logFile = engineEval $ "diary " <> (toFilePath logFile)

diaryOn :: HasEngine r => ZIO r MatlabException ()
diaryOn = engineEval "diary on"

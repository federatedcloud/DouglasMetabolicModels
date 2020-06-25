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

diaryFile :: HasEngine r => Path b File -> ZIO r MatlabException ()
diaryFile logFile = engineEval $ "diary " <> (toFilePath logFile)

diaryOn :: HasEngine r => ZIO r MatlabException ()
diaryOn = engineEval "diary on"

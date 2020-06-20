-- |Note, this module is almost all user-code and is
-- |a candidate for extraction.
module COBRA.MATLAB.Engine.Analysis where

import           COBRA.Syntax
import           Foreign.Matlab
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans

initCobraToolbox :: HasEngine r => ZIO r MatlabException ()
initCobraToolbox = engineEvalProc "initCobraToolbox" []
  

-- Ideally a UIO (Either String (Path Abs Dir)
pwd :: HasEngine r => ZIO r MatlabException (Path Abs Dir)
pwd = do
  pwdDirAnyArr <- headZ "pwd returned nothing" =<< engineEvalFun "pwd" [] 1
  pwdDirCArr <- castMXArray pwdDirAnyArr
  dir <- mxArrayGetAll pwdDirCArr
  mxleZ . zlift $ parseAbsDir dir

-- TODO: Move to haskell-matlab?
initHSMatlabEngineEnv :: HasEngine r => [ZIO r MatlabException ()] -> ZIO r MatlabException ()
initHSMatlabEngineEnv iFuns = do
  env <- ask
  let eng = getEngine env
  initDir <- pwd
  mxleZ . zlift $ putStrLn $ "Starting HS MATLAB initialization in " <> (toFilePath initDir)
  runAll iFuns


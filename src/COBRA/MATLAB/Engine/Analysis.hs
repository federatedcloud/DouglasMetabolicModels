-- |Note, this module is almost all user-code and is
-- |a candidate for extraction.
module COBRA.MATLAB.Engine.Analysis where

import COBRA.Syntax
import Foreign.Matlab
import Foreign.Matlab.Engine
import Path

initCobraToolbox :: Engine -> IO ()
initCobraToolbox eng = do
  engineEvalProc eng "initCobraToolbox" []
  

-- Ideally a UIO (Either String (Path Abs Dir)
pwd :: Engine -> IO (Path Abs Dir)
pwd eng = do
  [pwdDirAnyArr] <- engineEvalFun eng "pwd" [] 1
  pwdDirCArrMay <- castMXArray pwdDirAnyArr
  dirOrEmptyStr <- case pwdDirCArrMay of
    Just pwdDirCArr -> mxArrayGetAll pwdDirCArr
    Nothing -> pure ""
  parseAbsDir dirOrEmptyStr


-- TODO: Move to haskell-matlab?
initHSMatlabEngineEnv :: Engine -> [Engine -> IO ()] -> IO ()
initHSMatlabEngineEnv eng iFuns = do
  initDir <- pwd eng
  putStrLn $ "Starting HS MATLAB initialization in " <> (toFilePath initDir)
  runAll $ iFuns <*> [eng]


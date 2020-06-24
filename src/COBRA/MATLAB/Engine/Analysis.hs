-- |Note, this module is almost all user-code and is
-- |a candidate for extraction.
module COBRA.MATLAB.Engine.Analysis where

import           COBRA.Syntax
import           Control.Lens
import           Foreign.Matlab
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOEngine.Wrappers
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOTypes
import           Path
import           ZIO.Trans


class HasCobraDir env where
  getCobraDir :: env -> Path Abs Dir

class HasCobraDir env => SetCobraDir env where
  setCobraDir :: env -> Path Abs Dir -> env

  cobraDir :: Lens' env (Path Abs Dir)
  cobraDir = lens getCobraDir setCobraDir

initCobraToolbox :: (HasEngine r, HasCobraDir r) => ZIO r MatlabException ()
initCobraToolbox = do
  initDir <- pwd -- bracket start
  env <- ask
  let cDir = getCobraDir env
  cd cDir
  engineEvalProc "initCobraToolbox" []
  cd initDir -- bracket close


-- TODO: bracket on staying in initDir?
initHSMatlabEngineEnv :: (HasEngine r, HasCobraDir r) =>
  [ZIO r MatlabException ()] -> ZIO r MatlabException ()
initHSMatlabEngineEnv iFuns = do
  initDir <- pwd -- bracket start
  mxleZ . zlift $ putStrLn $ "Starting HS MATLAB initialization in " <> (toFilePath initDir)
  runAll iFuns
  cd initDir -- bracket close

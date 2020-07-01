module Main where
import           COBRA.PriorityEffects
import           Foreign.Matlab.ZIOEngine
import           Path
import           ZIO.Trans

main :: IO ()
main = do
  eng <- newEngine ""
  env <- pure $ Env {
      _eCobraDir = userCobraDir
    , _eProjDir = projectDir
    , _eAnalysisDir = projectDir </> analysisSubDir
    , _eEngine = eng
    , _eModelLoc = analysisModList
    }
  runApp app env
  where
    runApp a r = runZIO a r (putStrLn . show)


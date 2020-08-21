module Main where
import           COBRA.PriorityEffects
import           Foreign.Matlab.ZIOEngine
import           Path
import           ZIO.Trans

main :: IO ()
main = do
  eng <- newEngine ""
  runApp prioEffectAnalysis (defaultEnv eng)
  where
    runApp a r = runZIO a r (putStrLn . show)


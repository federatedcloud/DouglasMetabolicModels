{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad (join)
import           Data.Coerce (coerce)
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOEngine.Wrappers
import           Foreign.Matlab.ZIOTypes
import           COBRA
import           Data.Map.Lens
import qualified Data.Map.Strict as DM
import           Path
import           ZIO.Trans

data Env = Env {mEngine :: Engine}
  deriving Eq

instance HasEngine Env where
  getEngine = mEngine

instance SetEngine Env where
  setEngine env eng = env {mEngine = eng}

type AppEnv a = ZIO Env MatlabException a

zslift :: IO a -> ZIO r String a
zslift = (mapZError show) . zlift

main :: IO ()
main = do
  eng <- newEngine ""
  let env = Env eng
  runApp app env
  where
    runApp a r = runZIO a r (putStrLn . show)

app :: AppEnv ()
app = do
  env <- ask
  let eng = getEngine env
  diaryFile logFile
  diaryOn
  pl <- permListMX 5

  runAll $ disp <$> pl

  -- Pure Haskell version:
  -- let pl = permList 5
  -- runAll $ (putStrLn . show) <$> pl

  initHSMatlabEngineEnv [initDMM, initCobraToolbox]
  pure ()

logFile :: Path Rel File
logFile = $(mkRelFile "log_prioSims.txt")

initDMM :: AppEnv ()
initDMM = do
  engineEvalProc "initDMM" []

newtype MultiModel = MultiModel { _multiModel :: MStruct }
makeLenses '' MultiModel

newtype InfoCom = InfoCom { _infoCom :: MStruct }
makeLenses '' InfoCom

getInfoCom :: MultiModel -> AppEnv InfoCom
getInfoCom model = do
  infoComAA <- model ^. multiModel . mStruct . at "infoCom" & errMsgAt & liftEither & mxasZ
  infoComSA <- infoComAA & castMXArray
  infoComFirst <- infoComSA & mxArrayGetFirst
  pure $ InfoCom $ infoComFirst
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getInfoCom: couldn't find field"

newtype SpeciesAbbr = SpeciesAbbr { _speciesAbbr :: String }
makeLenses '' SpeciesAbbr

newtype StepResult = StepResult { _stepResult :: MStruct }
makeLenses '' StepResult

newtype ScheduleResult = ScheduleResult { _scheduleResult :: MXArray MCell }
makeLenses '' ScheduleResult

newtype SteadyComOpts = SteadyComOpts { _steadyComOpts :: MStruct }
makeLenses '' SteadyComOpts

type ModelMap = DM.Map SpeciesAbbr MultiModel

mmapToMStruct :: ModelMap -> AppEnv MStruct
mmapToMStruct m = do
  let sKeys = m & DM.keys <&> _speciesAbbr
  values <- m & DM.elems <&> (_multiModel >>> createMXScalar) & sequence
  (zip sKeys (anyMXArray <$> values) & DM.fromList) ^. from mStruct & pure

-- fromMStruct :: MStruct -> DM.Map String MAnyArray
-- fromMStruct ms = ms ^. mStruct

-- modelMap :: Iso' MStruct ModelMap
-- modelMap = iso to' from'
--   where
--     to' :: MStruct -> ModelMap
--     to' ms = ms ^. mStruct & DM.mapKeys SpeciesAbbr & DM.map MultiModel
--     from' :: ModelMap -> MStruct
--     from' mm = mm ^. mStruct


-- | Wraps a struct containing essentiality information
newtype EssInfo = EssInfo { _essInfo :: MStruct }
makeLenses '' EssInfo

data MediaType =
    Minimal
  | MinimalMerge
  | MinimalPlus
  | Rich
  | Unbounded

instance Show MediaType where
  show x = case x of
    Minimal -> "minimal"
    MinimalMerge -> "minimal-merge"
    MinimalPlus -> "minimal-plus"
    Rich -> "rich"
    Unbounded -> "unbounded"

semiDynamicSteadyCom :: ModelMap -- ^ List of multi-species models.
  -> [SpeciesAbbr]
  -> ScheduleResult
  -> Maybe SteadyComOpts
  -> VarArgIn
  -> AppEnv ScheduleResult

-- Base case
semiDynamicSteadyCom
  (_        :: ModelMap)
  ([]       :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (_        :: Maybe SteadyComOpts)
  (_        :: VarArgIn)
    = pure schedRes

-- Inductive case
semiDynamicSteadyCom
  (modelMap :: ModelMap)
  (currentSched:schedRemain :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (optsOverride :: Maybe SteadyComOpts)
  (varargin :: VarArgIn) = do
    arrays <- schedRes ^. scheduleResult & mxCellGetArraysOfType
    lastRes :: MStruct <- (mapLast mxArrayGetFirst arrays) & sequence
      >>= (errMsgSchedResEmpty >>> liftEither >>> mxasZ)
    pure schedRes -- TODO : change this to actual result
  where
    errMsgSchedResEmpty = mayToEi "semiDynamicSteadyCom: empty schedRes cell array"

-- | Helper function to determine how bounds are changed based on
-- | prior model state.
semiDynamicSteadyComUpdateBounds ::
     MultiModel
  -> MultiModel
  -> MXArray MDouble
  -> [EssInfo]
  -> AppEnv (MXArray MDouble)
semiDynamicSteadyComUpdateBounds
  (model :: MultiModel)
  (modelPrior :: MultiModel)
  (fluxPrior :: MXArray MDouble)
  (essInfo :: [EssInfo]) = do
  mxEssInfo <- fromListIO $ (coerce essInfo :: [MStruct])
  res <- engineEvalFun "semiDynamicSteadyComUpdateBounds" [
      EvalStruct $ model ^. multiModel
    , EvalStruct $ modelPrior ^. multiModel
    , EvalArray $ anyMXArray fluxPrior
    , EvalArray $ anyMXArray mxEssInfo
    ] 1 >>= headZ "No results from semiDynamicSteadyComUpdateBounds"
  castMXArray res

semiDynamicSteadyComStep ::
     MultiModel
  -> [SpeciesAbbr]
  -> SteadyComOpts
  -> [EssInfo]
  -> VarArgIn
  -> AppEnv StepResult
semiDynamicSteadyComStep
  (modelCom :: MultiModel)
  (currentSched :: [SpeciesAbbr])
  (optsOverride :: SteadyComOpts)
  (essentialRxns :: [EssInfo])
  (varargin :: VarArgIn) = do
  mxEssInfo <- fromListIO $ (coerce essentialRxns :: [MStruct])
  mxCurrentSched <- mxSchedule currentSched
  let mxVarargin = mxVarArgs varargin
  allArgs <- pure $ [
      EvalStruct $ modelCom ^. multiModel
    , EvalArray $ anyMXArray mxCurrentSched
    , EvalStruct $ optsOverride ^. steadyComOpts
    , EvalArray $ anyMXArray mxEssInfo
    ] ++ mxVarargin
  res <- engineEvalFun "semiDynamicSteadyComStep" allArgs 1
    >>= headZ "No results from semiDynamicSteadyComStep"
  resArr <- castMXArray res
  resStruct <- mxArrayGetFirst resArr
  pure $ StepResult $ resStruct

mxSchedule :: [SpeciesAbbr] -> AppEnv (MXArray MCell)
mxSchedule sched = cellFromListsIO $ (coerce sched :: [String])

checkEssentiality :: MultiModel -> [String] -> AppEnv [EssInfo]
checkEssentiality model rxns = do
  rxnsCA <- cellFromListsIO rxns
  res <- engineEvalFun "checkEssentiality" [
      EvalStruct $ model ^. multiModel
    , EvalArray $ anyMXArray rxnsCA] 1
    >>= headZ "No results from checkEssentiality"
  essArr <- castMXArray res
  listOfStructs <- mxArrayGetAll essArr
  pure $ EssInfo <$> listOfStructs

makeMultiModel :: [SpeciesAbbr] -> ModelMap -> MediaType -> AppEnv MultiModel
makeMultiModel modelKeys modMap mediaType = do
  species <- cellFromListsIO (coerce modelKeys :: [String])
  modMapStruct <- mmapToMStruct modMap
  res <- engineEvalFun "makeMultiModel" [
      EvalArray species
    , EvalStruct modMapStruct
    , EvalStr $ show $ mediaType] 1
    >>= headZ "No results from makeMultiModel"
  multiModelSA <- castMXArray res
  multiModelStruct <- mxArrayGetFirst multiModelSA
  pure $ MultiModel multiModelStruct


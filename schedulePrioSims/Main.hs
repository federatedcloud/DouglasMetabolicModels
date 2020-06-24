{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad (join)
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe)
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOEngine.Wrappers
import           Foreign.Matlab.ZIOTypes
import           COBRA
import           Data.List (intercalate)
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

newtype SpeciesAbbr = SpeciesAbbr { _speciesAbbr :: String }
makeLenses '' SpeciesAbbr

newtype StepResult = StepResult { _stepResult :: MStruct }
makeLenses '' StepResult

newtype ScomResult = ScomResult { _scomResult :: MStruct }
makeLenses '' ScomResult

newtype ScheduleResult = ScheduleResult { _scheduleResult :: MXArray MCell }
makeLenses '' ScheduleResult

newtype SteadyComOpts = SteadyComOpts { _steadyComOpts :: MStruct }
makeLenses '' SteadyComOpts

type ModelMap = DM.Map SpeciesAbbr MultiModel

getStepLast :: ScheduleResult -> AppEnv StepResult
getStepLast schedr = do
  lastCC <- schedr ^. scheduleResult & mxArrayGetLast >>= (mCell >>> castMXArray)
  lastCC & mxArrayGetFirst <&> StepResult

getSteps :: ScheduleResult -> AppEnv [StepResult]
getSteps schedr = do
  allCC :: [MStruct] <- schedr ^. scheduleResult & mxCellGetAllOfType
  allCC <&> StepResult & pure

stepsToSched :: [StepResult] -> AppEnv ScheduleResult
stepsToSched steps = do
  saList :: [MStructArray] <- steps <&> _stepResult & traverse createMXScalar -- & fromListIO <&> ScheduleResult
  saList <&> anyMXArray <&> MCell & fromListIO <&> ScheduleResult

getModel :: StepResult -> AppEnv MultiModel
getModel stepr = stepr ^. stepResult . mStruct . at "model" & errMsgAt
    & liftEither & mxasZ >>= castMXArray >>= mxArrayGetFirst <&> MultiModel
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getModel: couldn't find field"

getResult :: StepResult -> AppEnv ScomResult
getResult stepr = stepr ^. stepResult . mStruct . at "result" & errMsgAt
    & liftEither & mxasZ >>= castMXArray >>= mxArrayGetFirst <&> ScomResult
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getResult: couldn't find field"

getFlux :: ScomResult -> AppEnv (MXArray MDouble)
getFlux scres = scres ^. scomResult . mStruct . at "flux" & errMsgAt
    & liftEither & mxasZ >>= castMXArray
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getFlux: couldn't find field"

getInfoCom :: MultiModel -> AppEnv InfoCom
getInfoCom model = do
  infoComAA <- model ^. multiModel . mStruct . at "infoCom" & errMsgAt & liftEither & mxasZ
  infoComSA <- infoComAA & castMXArray
  infoComFirst <- infoComSA & mxArrayGetFirst
  pure $ InfoCom $ infoComFirst
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getInfoCom: couldn't find field"

getSpAbbr :: InfoCom -> AppEnv [SpeciesAbbr]
getSpAbbr icom = do
  saAA <- icom ^. infoCom . mStruct . at "spAbbr" & errMsgAt & liftEither & mxasZ
  saCA :: MXArray MCell <- saAA & castMXArray
  sas :: [String] <- mxCellGetAllListsOfType saCA
  pure $ coerce sas
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getSpAbbr: couldn't find field"


mmapToMStruct :: ModelMap -> AppEnv MStruct
mmapToMStruct m = do
  let sKeys = m & DM.keys <&> _speciesAbbr
  values <- m & DM.elems <&> (_multiModel >>> createMXScalar) & sequence
  (zip sKeys (anyMXArray <$> values) & DM.fromList) ^. from mStruct & pure

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

semiDynamicSteadyCom ::
     ModelMap -- ^ List of multi-species models.
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
    lastStepMay <- schedRes & getStepLast & mxToMaybeZ
    lastOrgKeys <- maybe (pure []) (getModel >=> getInfoCom >=> getSpAbbr) lastStepMay
    let currentOrgKeys = currentSched:lastOrgKeys
    (modelCom, mediaRxns) <- makeMultiModel currentOrgKeys modelMap MinimalPlus
    modelPrior <- maybe (pure modelCom) getModel lastStepMay
    commName <- commString modelCom
    essentialRxns <- checkEssentiality modelCom mediaRxns
    nSpecies <- modelCom & (getInfoCom >=> getSpAbbr) <&> length
    modelCom' <-
      if nSpecies > 1 then do
        lastStep <- schedRes & getStepLast
        fluxPrior <- (getResult >=> getFlux) lastStep
        newLB <- semiDynamicSteadyComUpdateBounds modelCom modelPrior fluxPrior essentialRxns
        modelCom & multiModel . mStruct . at "lb" ?~ (anyMXArray newLB) & pure
      else pure modelCom
    scOpts <- pure $ fromMaybe (SteadyComOpts mStructEmpty) optsOverride
    outStep <- semiDynamicSteadyComStep modelCom' [currentSched] scOpts essentialRxns varargin
    priorSteps <- getSteps schedRes
    stepsToSched $ outStep : priorSteps

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

makeMultiModel :: [SpeciesAbbr] -> ModelMap -> MediaType -> AppEnv (MultiModel, [String])
makeMultiModel modelKeys modMap mediaType = do
  species <- cellFromListsIO (coerce modelKeys :: [String])
  modMapStruct <- mmapToMStruct modMap
  resList <- engineEvalFun "makeMultiModel" [
      EvalArray species
    , EvalStruct modMapStruct
    , EvalString $ show $ mediaType] 2
  res <- headZ "No results from makeMultiModel" resList
  multiModelSA <- castMXArray res
  multiModelStruct <- mxArrayGetFirst multiModelSA
  rxns <- resList ^? ix 1 & errMsgIx1 & liftEither & mxasZ
    >>= castMXArray >>= mxCellGetAllListsOfType
  pure $ (MultiModel multiModelStruct, rxns)
  where
    errMsgIx1 :: Maybe a -> Either String a
    errMsgIx1 = mayToEi "makeMultiModel: couldn't find ix 1"



-- | Implemented directly instead of calling MATLAB function
commString :: MultiModel -> AppEnv String
commString mm = do
  infoCom <- getInfoCom mm
  spAbbrs <- getSpAbbr infoCom
  pure $ intercalate "_" $ coerce spAbbrs


runSemiDynamicSteadyCom ::
     ModelMap  -- ^ List of multi-species models.
  -> [SpeciesAbbr]
  -> Maybe SteadyComOpts
  -> AppEnv ScheduleResult

runSemiDynamicSteadyCom
  (modelMap :: ModelMap)
  (schedule :: [SpeciesAbbr])
  (optsOverride :: Maybe SteadyComOpts) = do
    origFeasTol <- getLpFeasTol
    setLpFeasTol 1e-8
    emptySchedRes <- ScheduleResult <$> fromListIO []
    schedRes <- semiDynamicSteadyCom
      modelMap
      schedule
      emptySchedRes
      optsOverride
      DM.empty
    setLpFeasTol origFeasTol
    pure schedRes

--TODO: use bracket to get/set feastol?
--TODO, but need a ZIO bracket based on: https://hackage.haskell.org/package/unexceptionalio-0.5.1/docs/UnexceptionalIO.html#v:bracket

getLpFeasTol :: AppEnv MDouble
getLpFeasTol = do
  res <- engineEvalFun "getCobraSolverParams" [
      EvalString "LP"
    , EvalString "feasTol"
    ] 1 >>= headZ "getLpFeasTol"
  castMXArray res >>= mxScalarGet

setLpFeasTol :: MDouble -> AppEnv ()
setLpFeasTol ft = do
  ftMX <- createMXScalar ft
  engineEvalProc "changeCobraSolverParams" [
      EvalString "LP"
    , EvalString "feasTol"
    , EvalArray ftMX
    ]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE LambdaCase         #-}

module Main where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad (join)
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock
import           Data.Time.Calendar
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOEngine
import           Foreign.Matlab.ZIOEngine.Wrappers
import           Foreign.Matlab.ZIOMAT
import           Foreign.Matlab.ZIOTypes
import           COBRA
import           Data.List (intercalate)
import           Data.Map.Lens
import qualified Data.Map.Strict as DM
import           Path
import           Turtle (decodeString)
import           Turtle.Prelude (mktree)
import           ZIO.Trans


--TODO: move these two to haskell-matlab or even ZIO; generalize to all string-wrapping errors
mxNothingAppE :: String -> EIO MatlabException a -> EIO MatlabException a
mxNothingAppE aps eio = catchError eio (\case
  MXNothing er -> throwError $ MXNothing $ aps <> " " <> er
  er -> throwError $ er
  )

mxNothingAppZ :: String -> ZIO r MatlabException a -> ZIO r MatlabException a
mxNothingAppZ aps zio = do
  env <- ask
  (ezlift . (mxNothingAppE aps) . (flip runReaderT env)  . _unZIO) zio


data ModelCoords = ModelCoords { _modelFile :: Path Rel File, _mapVar :: String}
  deriving Eq
makeLenses ''ModelCoords

newtype Model = Model { _model :: MStruct }
makeLenses ''Model

newtype MultiModel = MultiModel { _multiModel :: MStruct }
makeLenses ''MultiModel

newtype InfoCom = InfoCom { _infoCom :: MStruct }
makeLenses ''InfoCom

newtype SpeciesAbbr = SpeciesAbbr { _speciesAbbr :: String }
  deriving (Eq, Ord, Show)
makeLenses ''SpeciesAbbr

newtype StepResult = StepResult { _stepResult :: MStruct }
makeLenses ''StepResult

newtype ScomResult = ScomResult { _scomResult :: MStruct }
makeLenses ''ScomResult

newtype ScheduleResult = ScheduleResult { _scheduleResult :: MXArray MCell }
makeLenses ''ScheduleResult

newtype SteadyComOpts = SteadyComOpts { _steadyComOpts :: MStruct }
makeLenses ''SteadyComOpts

type ModelMap = DM.Map SpeciesAbbr Model

-- TODO: *** *** *** read these from Dhall *** *** *** ***

userCobraDir :: Path Abs Dir
userCobraDir = [absdir|/home/bebarker/workspace/cobratoolbox|]

projectDir :: Path Abs Dir
projectDir = [absdir|/home/bebarker/workspace/DouglasMetabolicModels|]

analysisModList :: ModelCoords
analysisModList = ModelCoords {
    _modelFile = [relfile|5.models_080719/all_5.mat|]
  , _mapVar = "allModelsMap"
  }


-- TODO End: ^^^ ^^^ read these from Dhall ^^^ ^^^ ^^^ ^^^


-- This one is OK hardcoded
analysisSubDir :: Path Rel Dir
analysisSubDir = [reldir|analysis/semiDynamicSC|]

-- This one is OK hardcoded
modelDir :: Path Rel Dir
modelDir = [reldir|models|]

-- This one is OK hardcoded
logFile :: Path Rel File
logFile = [relfile|log_prioSims.txt|]

data Env = Env {
    _eCobraDir :: Path Abs Dir
  , _eProjDir :: Path Abs Dir
  , _eAnalysisDir :: Path Abs Dir
  , _eEngine :: Engine
  , _eModelLoc :: ModelCoords
} deriving Eq
makeLenses ''Env

instance HasEngine Env where
  getEngine = _eEngine

instance SetEngine Env where
  setEngine env eng = env {_eEngine = eng}

instance HasCobraDir Env where
  getCobraDir = _eCobraDir

instance SetCobraDir Env where
  setCobraDir env cDir = env {_eCobraDir= cDir}

type AppEnv a = ZIO Env MatlabException a

zslift :: IO a -> ZIO r String a
zslift = (mapZError show) . zlift

initDMM :: AppEnv ()
initDMM = do
  initDir <- pwd -- bracket start
  env <- ask
  let pDir = _eProjDir env
  cd pDir
  engineEvalProc "initDMM" []
  cd initDir -- bracket close

getStepLast :: ScheduleResult -> AppEnv StepResult
getStepLast schedr = mxNothingAppZ "getStepLast" $ do
  printLn "DEBUG: entering getStepLast"
  -- lastCC <- schedr ^. scheduleResult & mxArrayGetLast >>= (mCell >>> castMXArray)
  -- TODO: ^break above line up to diagnose issue
  lastCell :: MCell <- schedr ^. scheduleResult & mxArrayGetLast
  printLn "DEBUG: performed mxArrayGetLast in getStepLast"
  let lastCellAA = mCell lastCell
  lastCellStructArray <- castMXArray lastCellAA
  printLn $ "DEBUG: isMNull lastCellStructArray?:" <> (show $ isMNull lastCellStructArray)
  lastCellStructArray & mxArrayGetFirst <&> StepResult

getSteps :: ScheduleResult -> AppEnv [StepResult]
getSteps schedr = do
  allCC :: [MStruct] <- schedr ^. scheduleResult & mxCellGetAllOfType
  allCC <&> StepResult & pure

stepsToSched :: [StepResult] -> AppEnv ScheduleResult
stepsToSched steps = do
  saList :: [MStructArray] <- steps <&> _stepResult & traverse createMXScalar -- & fromListIO <&> ScheduleResult
  saList <&> anyMXArray <&> MCell & fromListIO <&> ScheduleResult

getModel :: StepResult -> AppEnv MultiModel
getModel stepr = mxNothingAppZ "getModel" $
  stepr ^. stepResult . mStruct . at "model" & errMsgAt
    & liftEither & mxasZ >>= castMXArray >>= mxArrayGetFirst <&> MultiModel
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getModel: couldn't find field"

getResult :: StepResult -> AppEnv ScomResult
getResult stepr = mxNothingAppZ "getResult" $
  stepr ^. stepResult . mStruct . at "result" & errMsgAt
    & liftEither & mxasZ >>= castMXArray >>= mxArrayGetFirst <&> ScomResult
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getResult: couldn't find field"

getFlux :: ScomResult -> AppEnv (MXArray MDouble)
getFlux scres = mxNothingAppZ "getFlux" $
  scres ^. scomResult . mStruct . at "flux" & errMsgAt
    & liftEither & mxasZ >>= castMXArray
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getFlux: couldn't find field"

getInfoCom :: MultiModel -> AppEnv InfoCom
getInfoCom model = mxNothingAppZ "getInfoCom" $ do
  infoComAA <- model ^. multiModel . mStruct . at "infoCom" & errMsgAt & liftEither & mxasZ
  infoComSA <- infoComAA & castMXArray
  infoComFirst <- infoComSA & mxArrayGetFirst
  pure $ InfoCom $ infoComFirst
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getInfoCom: couldn't find field"

getSpAbbr :: InfoCom -> AppEnv [SpeciesAbbr]
getSpAbbr icom = mxNothingAppZ "getSpAbbr" $ do
  saAA <- icom ^. infoCom . mStruct . at "spAbbr" & errMsgAt & liftEither & mxasZ
  saCA :: MXArray MCell <- saAA & castMXArray
  sas :: [String] <- mxCellGetAllListsOfType saCA
  pure $ coerce sas
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getSpAbbr: couldn't find field"

mmapToMStruct :: ModelMap -> AppEnv MStruct
mmapToMStruct m = mxNothingAppZ "mmapToMStruct" $ do
  let sKeys = m & DM.keys <&> _speciesAbbr
  values <- m & DM.elems <&> (_model >>> createMXScalar) & sequence
  (zip sKeys (anyMXArray <$> values) & DM.fromList) ^. from mStruct & pure

structToMMap :: MStruct -> AppEnv ModelMap
structToMMap ms = mxNothingAppZ "structToMMap" $ do
  let mmapKeyed = ms ^. mStruct & DM.mapKeys SpeciesAbbr
  mmapKeyStructed <- traverse (castMXArray >=> mxArrayGetFirst) mmapKeyed
  pure $ DM.map Model mmapKeyStructed

readModelMap :: AppEnv ModelMap
readModelMap = mxNothingAppZ "readModelMap" $ do
  env <- ask
  let mFilePath = (env ^. eProjDir) </> modelDir </> (env ^. eModelLoc . modelFile)
  printLn $ "DEBUG: reading models file: " <> (toFilePath mFilePath)
  mFile <- matOpen mFilePath MATRead
  mmapAny <- matGet mFile (env ^. eModelLoc . mapVar)   -- DEBUG
  mmapAnyClass <- mxArrayClass mmapAny                  -- DEBUG
  printLn $ "DEBUG: class is " <> (show mmapAnyClass)
  mmapStruct <- matGet mFile (env ^. eModelLoc . mapVar)
    >>= castMXArray >>= mxArrayGetFirst
  structToMMap mmapStruct

-- | Wraps a struct containing essentiality information
newtype EssInfo = EssInfo { _essInfo :: MStruct }
makeLenses ''EssInfo

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
  sched@(currentSched:schedRemain :: [SpeciesAbbr])
  (schedRes :: ScheduleResult)
  (optsOverride :: Maybe SteadyComOpts)
  (varargin :: VarArgIn) = do
    printLn "DEBUG: semiDynamicSteadyCom entered (inductive case)"
    printLn $ "DEBUG: semiDynamicSteadyCom schedule is " <> (spAbbToCommName sched)
    lastStepMay <- schedRes & getStepLast & mxToMaybeZ
    printLn $ "DEBUG: semiDynamicSteadyCom: retrieved lastStepMay for  "
      <> (spAbbToCommName sched)
    lastOrgKeys <- maybe (pure []) (getModel >=> getInfoCom >=> getSpAbbr) lastStepMay
    let currentOrgKeys = currentSched:lastOrgKeys
    (modelCom, mediaRxns) <- makeMultiModel currentOrgKeys modelMap MinimalPlus
    modelPrior <- maybe (pure modelCom) getModel lastStepMay
    commName <- commString modelCom
    printLn $ "DEBUG: generated commName: " <> commName
    essentialRxns <- checkEssentiality modelCom mediaRxns
    nSpecies <- modelCom & (getInfoCom >=> getSpAbbr) <&> length
    modelCom' <-
      if nSpecies > 1 then do
        lastStep <- schedRes & getStepLast
        fluxPrior <- (getResult >=> getFlux) lastStep
        newLB <- semiDynamicSteadyComUpdateBounds modelCom modelPrior fluxPrior essentialRxns
        modelCom & multiModel . mStruct . at "lb" ?~ (anyMXArray newLB) & pure
      else pure modelCom
    printLn $ "DEBUG: created modelCom' "
    scOpts <- pure $ fromMaybe (SteadyComOpts mStructEmpty) optsOverride
    printLn $ "DEBUG: created scOpts"
    outStep <- semiDynamicSteadyComStep modelCom' [currentSched] scOpts essentialRxns varargin
    printLn $ "DEBUG: created outStep"
    priorSteps <- getSteps schedRes
    printLn $ "DEBUG: retrieved priorSteps"
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
  (essInfo :: [EssInfo]) = mxNothingAppZ "semiDynamicSteadyComUpdateBounds" $ do
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
  (varargin :: VarArgIn) = mxNothingAppZ "semiDynamicSteadyComStep" $ do
  printLn "DEBUG: entered semiDynamicSteadyComStep"
  mxEssInfo <- fromListIO $ (coerce essentialRxns :: [MStruct])
  printLn "DEBUG: retrieved mxEssInfo"
  mxCurrentSched <- mxSchedule currentSched
  printLn "DEBUG: retrieved mxCurrentSched"
  let mxVarargin = mxVarArgs varargin
  allArgs <- pure $ [
      EvalStruct $ modelCom ^. multiModel
    , EvalArray $ anyMXArray mxCurrentSched
    , EvalStruct $ optsOverride ^. steadyComOpts
    , EvalArray $ anyMXArray mxEssInfo
    ] ++ mxVarargin
  printLn "DEBUG: constructed allArgs"
  res <- engineEvalFun "semiDynamicSteadyComStep" allArgs 1
    >>= headZ "No results from semiDynamicSteadyComStep"
  resArr <- castMXArray res
  printLn "DEBUG: obtained resArr"
  resStruct <- mxArrayGetFirst resArr
  pure $ StepResult $ resStruct

mxSchedule :: [SpeciesAbbr] -> AppEnv (MXArray MCell)
mxSchedule sched = cellFromListsIO $ (coerce sched :: [String])

checkEssentiality :: MultiModel -> [String] -> AppEnv [EssInfo]
checkEssentiality model rxns = mxNothingAppZ "checkEssentiality" $ do
  rxnsCA <- cellFromListsIO rxns
  res <- engineEvalFun "checkEssentiality" [
      EvalStruct $ model ^. multiModel
    , EvalArray $ anyMXArray rxnsCA] 1
    >>= headZ "No results from checkEssentiality"
  essArr <- castMXArray res
  listOfStructs <- mxArrayGetAll essArr
  pure $ EssInfo <$> listOfStructs

makeMultiModel :: [SpeciesAbbr] -> ModelMap -> MediaType -> AppEnv (MultiModel, [String])
makeMultiModel modelKeys modMap mediaType = mxNothingAppZ "makeMultiModel" $ do
  species <- cellFromListsIO (coerce modelKeys :: [String])
  modMapStruct <- mmapToMStruct modMap
  resList <- engineEvalFun "makeMultiModel" [
      EvalArray species
    , EvalStruct modMapStruct
    , EvalString $ show $ mediaType] 2
  res <- headZ "No results from makeMultiModel" resList
  multiModelSA <- mxNothingAppZ "multiModelSA" $ castMXArray res
  multiModelStruct <- mxArrayGetFirst multiModelSA
  rxns <- mxNothingAppZ "retrieving rxns" $
    resList ^? ix 1 & errMsgIx1 & liftEither & mxasZ
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
  pure $ spAbbToCommName spAbbrs

spAbbToCommName :: [SpeciesAbbr] -> String
spAbbToCommName sps = intercalate "_" $ coerce sps

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
    printLn "DEBUG: first call to setLpFeasTol completed"
    emptySchedRes <- ScheduleResult <$> fromListIO []
    printLn "DEBUG: created emptySchedRes"
    schedRes <- semiDynamicSteadyCom
      modelMap
      schedule
      emptySchedRes
      optsOverride
      DM.empty
    setLpFeasTol origFeasTol
    pure schedRes

--TODO: use bracket to get/set feastol in runSemiDynamicSteadyCom?
--TODO, but need a ZIO bracket based on: https://hackage.haskell.org/package/unexceptionalio-0.5.1/docs/UnexceptionalIO.html#v:bracket

getLpFeasTol :: AppEnv MDouble
getLpFeasTol = mxNothingAppZ "getLpFeasTol" $ do
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

saveSchedRes :: Path b Dir -> [SpeciesAbbr] -> ScheduleResult -> AppEnv ()
saveSchedRes resDir schedule sr = do
  let cName = spAbbToCommName schedule
  let fileName = cName <> ".mat"
  fileRel <- mxaeZ $ zlift $ parseRelFile fileName
  matSave (resDir </> fileRel) [(cName, sr ^. scheduleResult)]

-- | Creates a results folder under the analysis folder and returns its Path.
makeResultFolder :: AppEnv (Path Abs Dir)
makeResultFolder = do
  env <- ask
  date <- mxaeZ $ zlift $ getCurrentTime
  uuid <- mxaeZ $ zlift $ UUID4.nextRandom
  let dateTup = date & utctDay & toGregorian
  dateStr <- pure $ intercalate "_" [
      dateTup ^. _1 & show
    , dateTup ^. _2 & show
    , dateTup ^. _3 & show
    ]
  let folderStr = dateStr <> "__" <> (UUID.toString uuid)
  resFolder <- mxaeZ $ zlift $ parseRelDir folderStr
  let resPath = (env ^. eAnalysisDir) </> resFolder
  mkdirp resPath
  pure resPath

-- A util function
mkdirp :: Path b Dir -> AppEnv ()
mkdirp p = mxaeZ $ zlift $ mktree $ decodeString $ toFilePath p


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

app :: AppEnv ()
app = do
  env <- ask
  let eng = getEngine env
  let analysisDir = _eAnalysisDir env
  resFolder <- makeResultFolder
  diaryFile $ resFolder </> logFile
  diaryOn
  pl <- permListMX 5

  runAll $ disp <$> pl

  -- Pure Haskell version:
  -- let pl = permList 5
  -- runAll $ (putStrLn . show) <$> pl

  initHSMatlabEngineEnv [initDMM, initCobraToolbox]

  all5map <- readModelMap
  let schedule = all5map & DM.keys
  printLn $ "DEBUG: schedule is " <> (spAbbToCommName schedule)
  schedRes <- runSemiDynamicSteadyCom all5map schedule Nothing
  saveSchedRes resFolder schedule schedRes

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module COBRA.PriorityEffects where

import           Prelude hiding (appendFile, writeFile)
import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad (join)
import           Data.Bifunctor
import           Data.Coerce (coerce)
import           Data.Foldable
import Data.List (permutations)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Sort (sortOn)
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
import           Debug.Trace (trace)
import           Path
import           System.Environment (getArgs)
import           Turtle (decodeString)
import           Turtle.Prelude (mktree)
import           ZIO.Trans


-- TODO: Extract this to a very simple library?

type CsvMat = DM.Map (Integer, Integer) String

data CsvMatConf = CsvMatConf {
  delim :: String
, emptyCell :: String
} deriving (Eq, Show)

defCsvMatConf :: CsvMatConf
defCsvMatConf = CsvMatConf {
  delim = ","
, emptyCell = ""
}

class HasCsvMatConf r where
  getCsvMatConf :: r -> CsvMatConf

writeCsv :: forall b r. HasCsvMatConf r
  => CsvMat -> Path b File -> ZIO r SomeNonPseudoException ()
writeCsv cmat fPath = do
  writeFile fPath ""
  go indices
  where
    minFold :: Foldable t => t Integer -> Integer
    minFold = foldr min 0
    maxFold :: Foldable t => t Integer -> Integer
    maxFold = foldr max 0
    minRow = cmat & DM.keys <&> fst & minFold
    maxRow = cmat & DM.keys <&> fst & maxFold
    minCol = cmat & DM.keys <&> snd & minFold
    maxCol = cmat & DM.keys <&> snd & maxFold
    indices = [ (x,y) | x <- [minRow..maxRow], y <- [minCol..maxCol] ]
    go :: [(Integer, Integer)] -> ZIO r SomeNonPseudoException ()
    go [] = pure ()
    go (ix:ixs) = do
      cmConf <- ask <&> getCsvMatConf
      let (rowIxs, restIxs) = span (\i -> fst i == fst ix) (ix:ixs)
      let entryStrs = (\i -> DM.findWithDefault (emptyCell cmConf) i cmat) <$> rowIxs
      let row = intercalate (delim cmConf) entryStrs
      appendFile fPath (row <> "\n")
      go restIxs

---- End CSV lib


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

type RxnFluxMap = DM.Map String MDouble
type SchedAndOrgs = ([SpeciesAbbr], [SpeciesAbbr])
type SimFluxMap = DM.Map SchedAndOrgs RxnFluxMap

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
analysisSubDir = [reldir|analysis/PriorityEffects/semiDynamicSC|]

-- This one is OK hardcoded
modelDir :: Path Rel Dir
modelDir = [reldir|models|]

logFile :: Path Rel File
logFile = [relfile|log_prioSims.txt|]

logFileAnalysis :: Path Rel File
logFileAnalysis = [relfile|log_prioSimsAnalysis.txt|]

heatMapAllFluxFile :: Path Rel File
heatMapAllFluxFile = [relfile|heatMap_allFlux.csv|]

data Env = Env {
    _eCobraDir :: Path Abs Dir
  , _eProjDir :: Path Abs Dir
  , _eAnalysisDir :: Path Abs Dir
  , _eEngine :: Engine
  , _eModelLoc :: ModelCoords
  , _csvMatConf :: CsvMatConf
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

instance HasCsvMatConf Env where
  getCsvMatConf = _csvMatConf

zslift :: IO a -> ZIO r String a
zslift = (mapZError show) . zlift

type AppEnv a = ZIO Env MatlabException a

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
  lastCC <- schedr ^. scheduleResult & mxArrayGetFirst >>= (mCell >>> castMXArray)
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
getModel stepr = mxNothingAppZ "getModel" $
  stepr ^. stepResult . mStruct . at "model" & errMsgAt
    & liftEither & mxasZ >>= castMXArray >>= mxArrayGetFirst <&> MultiModel
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getModel: couldn't find field"

getRxns :: MultiModel -> AppEnv [String]
getRxns model = mxNothingAppZ "getRxns" $ do
  rxnsAA <- model ^. multiModel . mStruct . at "rxns" & errMsgAt & liftEither & mxasZ
  rxnsCA <- rxnsAA & castMXArray
  mxCellGetAllListsOfType rxnsCA
  where
    errMsgAt :: Maybe a -> Either String a
    errMsgAt = mayToEi "getRxns: couldn't find field"

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

getFluxMapAndSp :: StepResult -> AppEnv ([SpeciesAbbr], RxnFluxMap)
getFluxMapAndSp stepr = mxNothingAppZ "getFluxMapAndSp" $ do
  result <- getResult stepr
  mxFlux <- getFlux result
  flux <- mxArrayGetAll mxFlux
  model <- getModel stepr
  rxns <- getRxns model
  let fMap = DM.fromList $ zip rxns flux
  spAbbrs <- (getInfoCom >=> getSpAbbr) model
  pure (spAbbrs, fMap)

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
    printLn $ "DEBUG: semiDynamicSteadyCom schedule is " <> (spAbbToCommName sched)
    lastStepMay <- schedRes & getStepLast & mxToMaybeZ
    printLn $ "DEBUG: semiDynamicSteadyCom: retrieved lastStepMay for  "
      <> (spAbbToCommName sched)
    lastOrgKeys <- maybe (pure []) (getModel >=> getInfoCom >=> getSpAbbr) lastStepMay
    let currentOrgKeys = currentSched:lastOrgKeys
    (modelCom, mediaRxns) <- makeMultiModel currentOrgKeys modelMap MinimalPlus -- TODO: remove media hardcoding
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
    newSchedRes <- stepsToSched $ outStep : priorSteps
    semiDynamicSteadyCom
      modelMap
      schedRemain
      newSchedRes
      optsOverride
      DM.empty

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
    setLpFeasTol 1e-5
    emptySchedRes <- ScheduleResult <$> fromListIO []
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

saveSchedRes :: Path b Dir -> [SpeciesAbbr] -> [ScheduleResult] -> AppEnv ()
saveSchedRes resDir orgs srs = do
  let cName = spAbbToCommName orgs
  let fileName = cName <> ".mat"
  fileRel <- mxaeZ $ zlift $ parseRelFile fileName
  namesAndRes <- forM srs \sr -> do
    lastStepMay <- sr & getStepLast & mxToMaybeZ
    lastOrgKeys <- maybe (pure []) (getModel >=> getInfoCom >=> getSpAbbr) lastStepMay
    let stepName = spAbbToCommName lastOrgKeys
    pure $ (stepName, sr ^. scheduleResult)
  matSave (resDir </> fileRel) namesAndRes

loadSchedRes :: Path b File -> AppEnv [ScheduleResult]
loadSchedRes resFile = do
  varAAs <- (fmap snd) <$> matLoad resFile
  varCAs <- traverse castMXArray varAAs
  pure $ ScheduleResult <$> varCAs
  

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

getResultPath :: String -> AppEnv (Path Abs Dir)
getResultPath folderStr = do
  env <- ask
  resDir <- mxaeZ $ zlift $ parseRelDir folderStr
  let resPath = (env ^. eAnalysisDir) </> resDir
  pure resPath

-- A util function
mkdirp :: Path b Dir -> AppEnv ()
mkdirp p = mxaeZ $ zlift $ mktree $ decodeString $ toFilePath p

schedulePrioSims :: AppEnv ()
schedulePrioSims = do
  env <- ask
  let eng = getEngine env
  resFolder <- makeResultFolder
  diaryFile $ resFolder </> logFile
  diaryOn

  initHSMatlabEngineEnv [initDMM, initCobraToolbox]
  
  -- FIXME: need to properly handle objects in haskell-matlab
  -- rxGroupTest <- readRxnGroups Transport
  -- printLn $ show rxGroupTest

  all5map <- readModelMap
  -- let orgs = all5map & DM.keys
  let orgs = coerce ["AF", "AP", "AT", "LB", "LP"]
  -- let allScheds = permutations orgs
  -- LP_AP_AT_AF_LB
  let allScheds = [coerce $ ["LP", "AP", "AT", "AF", "LB"]] -- DEBUG
  printLn $ "DEBUG: organism set is " <> (spAbbToCommName orgs)
  allSchedRes <- forM allScheds $ \sched -> runSemiDynamicSteadyCom all5map sched Nothing
  saveSchedRes resFolder orgs allSchedRes


prioEffectAnalysis :: AppEnv ()
prioEffectAnalysis = do
  args <- mxaeZ $ zlift $ getArgs
  case args of
    (resDirStr:schedResFileStr:_) -> do
      resPath <- getResultPath resDirStr
      diaryFile $ resPath </> logFileAnalysis
      diaryOn

      -- Shouldn't need COBRA Toolobox for this:
      initHSMatlabEngineEnv [initDMM]
      schedResFile <- mxaeZ $ zlift $ parseRelFile schedResFileStr
        
      let srFile = resPath </> schedResFile
      allSchedRes <- loadSchedRes srFile
      sfMap <- makeFluxMap allSchedRes
      srHead <- headZ "No Schedule Results loaded" allSchedRes
      lastStep <- srHead & getStepLast
      modelWithAllOrgs <- getModel lastStep
      rxns <- getRxns modelWithAllOrgs
      let tbl = makeFluxTable sfMap rxns
      mxaeZ $ writeCsv tbl (resPath </> heatMapAllFluxFile)
    _ -> throwError $ MXNothing "Need at least 2 arguments (result folder, mat file)"

  
defaultEnv :: Engine -> Env
defaultEnv eng = Env {
  _eCobraDir = userCobraDir
, _eProjDir = projectDir
, _eAnalysisDir = projectDir </> analysisSubDir
, _eEngine = eng
, _eModelLoc = analysisModList
, _csvMatConf = defCsvMatConf
}

--- HeatMap Table

makeFluxMap :: [ScheduleResult] -> AppEnv SimFluxMap
makeFluxMap allScheds = foldrM insertSFM DM.empty allScheds
  where
    insertSFM :: ScheduleResult -> SimFluxMap -> AppEnv SimFluxMap
    insertSFM sr sfm = do
      lastStepMay <- sr & getStepLast & mxToMaybeZ
      steps <- getSteps sr
      lastOrgKeys <- maybe (pure []) (getModel >=> getInfoCom >=> getSpAbbr) lastStepMay
      spAbbrsAndFmaps <- traverse getFluxMapAndSp steps
      let stepMap = DM.fromList $ (first (\sps -> (lastOrgKeys, sps))) <$> spAbbrsAndFmaps
      pure $ DM.union stepMap sfm



makeFluxTable :: SimFluxMap -> [String] -> CsvMat
makeFluxTable sfMap orderedRxns = foldr updateTable tblCRH sfKeys
  where
    tblCH = applyHeaders mkColHs colHeaders DM.empty
    tblCRH = applyHeaders mkRowHs rowHeaders tblCH
    rxnIx = DM.fromList $ zip orderedRxns [0..]
    sfKeys0 = sortOn (second length) $ DM.keys sfMap
    sfKeys = trace ("keys are: " <> (show (coerce sfKeys0 :: [([String], [String])]))) sfKeys0
    sfKeyIx = DM.fromList $ zip sfKeys [0..]
    sfHeaderTups = (\k -> (spAbbToCommName $ fst k, spAbbToCommName $ snd k)) <$> sfKeys
    schedHeader = fst <$> sfHeaderTups
    commHeader = snd <$> sfHeaderTups
    rowHeaders = [schedHeader, commHeader]
    colHeaders = [orderedRxns]
    nRxns = length orderedRxns
    mkIndex :: SchedAndOrgs -> String -> (Integer, Integer)
    mkIndex schedAndOrgs rxn =
      (nColHeads + sfKeyIx DM.! schedAndOrgs, nRowHeads + rxnIx DM.! rxn)
    nRowHeads = toInteger $ length rowHeaders
    nColHeads = toInteger $ length colHeaders
    applyHeaders :: (Integer -> [String] -> CsvMat) -> [[String]] -> CsvMat -> CsvMat
    applyHeaders go hss tbl = DM.union
      (DM.unions $ (\hs -> go (fst hs) (snd hs)) <$> hsIxed hss)
      tbl
    hsIxed hs = zip [0..] hs
    mkColHs :: Integer -> [String] -> CsvMat
    mkColHs ix hs = DM.fromList $ zip (zip (repeat ix) [nRowHeads..]) hs
    mkRowHs :: Integer -> [String] -> CsvMat
    mkRowHs ix hs = DM.fromList $ zip (zip [nColHeads..] (repeat ix)) hs
    updateTable :: SchedAndOrgs -> CsvMat -> CsvMat
    updateTable soKey tbl = foldr updateTblInner tbl fmRxns
      where
        fluxMap = sfMap DM.! soKey
        fmRxns = DM.keys fluxMap
        updateTblInner :: String -> CsvMat -> CsvMat
        updateTblInner rxn t = DM.insert
          (mkIndex soKey rxn)
          (show $ fluxMap DM.! rxn)
          t

-- FIXME: not working yet due to underlying library code issue (haskell-matlab handling containers.Map)
readRxnGroups :: ReactionType -> AppEnv (DM.Map String [String])
readRxnGroups rxnType = do
  resMMap :: MXMap <- engineEvalFun mxRgFun [] 1
    >>= headZ "No results from readTrRxnGroup" <&> MXMap
  rxnGrpMapRaw <- mmapToHmap resMMap
  traverse mxCellGetAllListsOfType rxnGrpMapRaw
  where
    mxRgFun :: String
    mxRgFun = case rxnType of
      Exchange -> "readExRxnGroups"
      Transport -> "readTrRxnGroups"

-- genHMTable :: rxnGroups, isTrans



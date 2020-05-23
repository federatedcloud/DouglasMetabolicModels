function schedRes = semiDynamicSteadyCom(modelMap, schedule, schedRes, optsOverride, varargin)
% This is a prototype of the intermediate-level (recursive) function that will be implemented in Haskell,
% which calls the step function to get work done in MATLAB.
% We output a cell array instead of a Map for better FFI

  currentOrgKeys = {};
  if numel(schedRes) > 0
    currentOrgKeys = split(schedRes{end}.model.infoCom.spAbbr, "_");
  end

  % Currently, a schedule has a single organism at each step:
  currentSched = schedule{1};
  currentOrgKeys = {currentOrgKeys{:}, currentSched};

  [modelCom, mediaRxns] = makeMultiModel(currentOrgKeys, modelMap, 'minimal-plus');

  modelPrior = {};
  if numel(schedRes) > 0
    modelPrior = schedRes{end}.model;
  else
    modelPrior = modelCom;
  end

  commName = commString(modelCom);
  essentialRxns = checkEssentiality(modelCom, mediaRxns);
  nSpecies = numel(modelCom.infoCom.spAbbr);

  if nSpecies > 1
    modelCom.lb = semiDynamicSteadComUpdateBounds( ...
      modelCom, modelPrior, schedRes{end}.result.flux, essentialRxns);
  end

  outStruct = semiDynamicSteadyComStep(modelCom, currentSched, optsOverride, essentialRxns, varargin{:})
  schedRes{end+1} = outStruct;

  if numel(schedule) > 1
    schedRes = semiDynamicSteadyCom(modelMap, schedule(2:end), schedRes, optsOverride, varargin{:});
  end
    
end

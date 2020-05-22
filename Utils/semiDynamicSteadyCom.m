function schedRes = semiDynamicSteadyCom(modelMap, schedule, optsOverride, varargin)
% This is a prototype of the outer (recursive) function that will be implemented in Haskell,
% which calls the step function to get work done in MATLAB

%TODO: checkEssentiality should be called here
%TODO: Inputs should also be calculated here, including the model
  numSteps = length(schedule);

  initCom = modelMap(schedule{1});

  currentOrgKeys = {};
  fluxPrior = [];

  modelCom = {};
  modelPrior = {};

  % We output a struct instead of a Map for better FFI
  schedRes = struct;

  for ii = 1:numSteps
    if ii > 1
      modelPrior = modelCom;
    end
    % Currently, a schedule has a single organism at each step:
    currentSched = schedule{ii};
    currentOrgKeys = union(currentOrgKeys, {currentSched});

    [modelCom, mediaRxns] = makeMultiModel(currentOrgKeys, modelMap, 'minimal-plus');
    commName = commString(modelCom);
    essentialRxns = checkEssentiality(modelCom, mediaRxns);

    if ii > 1
      modelCom.lb = semiDynamicSteadComUpdateBounds(modelCom, modelPrior, fluxPrior, essentialRxns);
    end

    outStruct = semiDynamicSteadyComStep(modelCom, currentSched, optsOverride, essentialRxns, varargin{:})
    schedRes = setfield(schedRes, commName, outStruct);
    
    
    fluxPrior = outStruct.result.flux;
  end
  
end

function schedRes = semiDynamicSteadyCom(modelMap, schedule, options, varargin)
% Find the maximum community growth rate at community steady-state using the `SteadyCom` algorithm
% using the following heuristic assumptions:
% 1. Start with an initial organsim (species) and media (media is implied by the initial model
% 2. Add a subsequent organsim, but change nutrient availability depending on whether something was produced/consumed
%    in the prior iteration
% 3. Repeat until all organisms have been added.


% TODO: at what level do we handle Memoization, e.g. https://wiki.haskell.org/Memoization
%     : I'm currently thinking we want to do the memoization in MATLAB to allow (hopefully)
%     : different schedules to use the same computed result, even if executed by different FFI calls

  fluxThresh = 1e-7;
  growThresh = 1e-7;
  
  numSteps = length(schedule);

  initCom = modelMap(schedule(1));

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
    currentSched = schedule(ii);
    currentOrgKeys = union(currentOrgKeys, {currentSched});

    modelCom = makeMultiModel(currentOrgKeys, modelMap, 'minimal-plus');
    commName = commStr(modelCom);
    essentialRxns = checkEssentiality(modelCom, rxns);

    if ii > 1
      modelCom.lb = updateLB(modelCom, modelPrior, essInfo)
    end

    [sol result LP LPminNorm] = SteadyCom(modelCom, schedule, options, varargin{:});
    schedRes.(commName) = struct(           ...
      'sol', sol,                           ...
      'result', result,                     ...
      'LP', LP,                             ...
      'LPminNorm', LPminNorm,               ...
      'essentialRxns', essentialRxns,       ...
      'newOrgs', {currentSched}             ...
    );
    
    fluxPrior = result.flux;
    result.essential = essentialRxns;
  end


  # TODO: priorFlux should probably be calculated as the minFVA value
  function newLB = updateLB(model, modelPrior, essInfo)
    newLB = model.lb;

    for ri == 1:length(essInfo)
      essi = essInfo{ii};
      % If it is not essential:
      if (essi.scomGrowth > growThresh)
	rxn = essi.rxn;
	rxnIxPrior = find(strcmp(modelPrior.rxns, rxn));
	rxnIxCurr = find(strcmp(model.rxns, rxn));

        % It is being consumed priorly, so we constrain to zero
	if fluxPrior(rxnIxPrior) < -fluxThresh
          newLB(rxnIxCurr) = 0;
        end
      end
    end
  end
  
end

function [sol, result, LP, LPminNorm, indLP] = semiDynamicSteadyCom(modelMap, schedule, options, varargin)
% Find the maximum community growth rate at community steady-state using the `SteadyCom` algorithm
% using the following heuristic assumptions:
% 1. Start with an initial organsim (species) and media (media is implied by the initial model
% 2. Add a subsequent organsim, but change nutrient availability depending on whether something was produced/consumed
%    in the prior iteration
% TODO: what about essential nutrients?
% 3. Repeat until all organisms have been added.


% TODO: at what level do we handle Memoization, e.g. https://wiki.haskell.org/Memoization

  fluxThresh = 1e-7;
  growThresh = 1e-7;
  
  numSteps = length(schedule);

  initCom = modelMap(schedule(1));

  currentOrgKeys = {};
  fluxPrior = [];

  modelLast = {};

  for ii = 1:numSteps
    % Currently, a schedule has a single organism at each step:
    currentOrgKeys = union(currentOrgKeys, {schedule(ii)});

    modelCom = makeMultiModel(currentOrgKeys, modelMap, 'minimal-plus');
    essentialRxns = checkEssentiality(modelCom, rxns);

    if ii > 1
      modelCom.lb = updateLB(modelCom, modelPrior, essInfo)
    end

    [sol result LP LPminNorm] = SteadyCom(modelCom, schedule, options, varargin);
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

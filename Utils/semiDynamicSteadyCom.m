function [sol, result, LP, LPminNorm, indLP] = semiDynamicSteadyCom(modelMap, schedule, options, varargin)
% Find the maximum community growth rate at community steady-state using the `SteadyCom` algorithm
% using the following heuristic assumptions:
% 1. Start with an initial organsim (species) and media (media is implied by the initial model
% 2. Add a subsequent organsim, but change nutrient availability depending on whether something was produced/consumed
%    in the prior iteration
% TODO: what about essential nutrients?
% 3. Repeat until all organisms have been added.


% TODO: at what level do we handle Memoization, e.g. https://wiki.haskell.org/Memoization

numSteps = length(schedule);

initCom = modelMap(schedule(1));

currentOrgKeys = {};
currentBounds = [];

for ii = 1:numSteps
  % Currently, a schedule has a single organism at each step:
  currentOrgKeys = union(currentOrgKeys, {schedule(ii)});

  modelCom = makeMultiModel(currentOrgKeys, modelMap, 'unbounded');

  %TODO: need to adjust this to work for exchange-reactions only, etc.
  modelCom = updateBounds(modelCom, rxns, lbs);
  currentBounds = modelMap(schedule(ii)); % TODO: probalby need an aux function for this for ii /= 1.
  

  [sol result LP LPminNorm] = SteadyCom(modelCom, schedule, options, varargin);
  
end
							 
  

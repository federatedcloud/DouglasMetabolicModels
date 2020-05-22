function outStruct = semiDynamicSteadyComStep(modelCom, currentSched, optsOverride, essentialRxns, varargin)
%
% TODO: varargin can be modeled as a Map in haskell and dynamically converted to arguments in haskell
%
%
% Find the maximum community growth rate at community steady-state using the `SteadyCom` algorithm
% using the following heuristic assumptions:
% 1. Start with an initial organsim (species) and media (media is implied by the initial model
% 2. Add a subsequent organsim, but change nutrient availability depending on whether something was produced/consumed
%    in the prior iteration
% 3. Repeat until all organisms have been added.


% TODO: at what level do we handle Memoization, e.g. https://wiki.haskell.org/Memoization
%     : I'm currently thinking we want to do the memoization in MATLAB to allow (hopefully)
%     : different schedules to use the same computed result, even if executed by different FFI calls.
%     : The proper idea here is partial schedules. This would require changing this to be a recursive function.
%     : But can we reorganize it so that the recursion is done in Haskell? First we should prototype it here
%     : and see if get basically the same results.
%     : For abstract types in haskell-matlab, it may be more efficient (or easier) if the hashing function is
%     : implemented in MATLAB.
%
% NOTE: checkEssentiality(modelCom, mediaRxns) also needs to be memoized.
 
  options = steadyComDefs(modelCom);
  if nargin > 2
    options = mergeStructs(options, optsOverride);
  end

  [sol result LP LPminNorm] = SteadyCom(modelCom, options, varargin{:});
  outStruct = struct(                     ...
    'sol', sol,                           ...
    'result', result,                     ...
    'LP', LP,                             ...
    'LPminNorm', LPminNorm,               ...
    'essentialRxns', {essentialRxns},     ... % gave to wrap this in {} or else struct-arrayified
    'newOrgs', {currentSched},            ...
    'model', modelCom                     ...
  );
end

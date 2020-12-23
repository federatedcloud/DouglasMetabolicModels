% Note: currently no flux comparison is done, just checks
% if growth stops, and records the fact.
%
% childRes  In this sense has a flux derived from a model with
%           one fewer permitted rxn flux than the parentRes.
% parentRes See childRes.
%
function pairAnalysis = compareFluxes(childResList, parentRes)

  parGrows = all(parentRes.res.BM);
  assert(parGrows);
  isMinimal = all(cellfun(@(f) ~allGrowing(f), childResList));
  pairAnalysis = struct;
  pairAnalysis.minimal = isMinimal;
  pairAnalysis.minRxns = parentRes.rxnsKept;

end

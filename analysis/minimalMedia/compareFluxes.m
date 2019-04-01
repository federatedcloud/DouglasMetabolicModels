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
  childgrenGrow = all(childResList.res.BM);
  pairAnalysis = struct;
  pairAnalysis.minimal = ~childrenGrow;
  pairAnalysis.minRxns = parentRes.rxnsKept;

end

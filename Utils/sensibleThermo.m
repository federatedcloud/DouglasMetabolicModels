% Enforces some thermodynamic constraints that should always be
% true in biological systems.
%
% function multiModel = sensibleThermo(multiModel)
function multiModel = sensibleThermo(multiModel)
  [atpSynthaseRxnNames, atpSynthaseIdxs]  = ...
    filter1d(@(r) contains(r, 'ATP synthase'), multiModel.rxnNames);
  atpSynthaseRxns = multiModel.rxns(atpSynthaseIdxs);
  effluxByRxn = {'EX_co2[u]'};
  influxByRxn = {'EX_o2[u]'};
  forwardOnlyRxns = {atpSynthaseRxns{:} effluxByRxn{:}};
  backwardOnlyRxns = {influxByRxn{:}};
  multiModel = changeRxnBounds(multiModel, forwardOnlyRxns, 0, 'l');
  multiModel = changeRxnBounds(multiModel, backwardOnlyRxns, 0, 'u');
end

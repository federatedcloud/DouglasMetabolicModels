% For a given flux vector, from a multi-species model, determines the competition
% index for each reacion, the interpretation of which is dependent on the number
% of species present in the model.
function mapOut = competitionIndices(rxns, flux)

  % transRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^[A-Z]{2}IEX.*tr$')));
  exRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^EX.*\[u\]$')));
  activeRxns = cellFlatMap(@(r) competitionIndex(r, rxns, flux), exRxns);
  cIndex = numel(activeRxns);
  activeRxnsStrs = cellFlatMap(@(c) strjoin(c, ', '), activeRxns);
  mapOut = activeRxnsStrs; %FIXME

end

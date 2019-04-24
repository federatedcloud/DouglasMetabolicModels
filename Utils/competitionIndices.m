% For a given flux vector, from a multi-species model, determines the competition
% index for each reacion, the interpretation of which is dependent on the number
% of species present in the model.
function dataOut = competitionIndices(model, flux)
  rxns = model.rxns;

  % transRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^[A-Z]{2}IEX.*tr$')));
  exRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^EX.*\[u\]$')));
  dataLines = cellFlatMap(@(r) competitionIndex(r, model, flux), exRxns);
  dataLines = filter1d(@(l) numel(l) > 0, dataLines);
  class(dataLines)
  dataOut = strjoin(dataLines, '\n');
  header = 'Met, CompIdx, InfluxIdx, OutfluxIdx, FluxInfo';
  dataOut = strjoin({header, dataOut}, '\n');

end

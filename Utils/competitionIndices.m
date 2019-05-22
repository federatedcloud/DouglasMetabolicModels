% For a given flux vector, from a multi-species model, determines the competition
% index for each reacion, the interpretation of which is dependent on the number
% of species present in the model.
function [dataOut, overlappingInputs] = competitionIndices(model, flux)
  rxns = model.rxns;
  nSpecies = numel(model.infoCom.spAbbr);

  % transRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^[A-Z]{2}IEX.*tr$')));
  exRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^EX.*\[u\]$')));
  compIdxOut = cellFlatMap(@(r) competitionIndex(r, model, flux), exRxns);
  dataLines = cellFlatMap(@(x) x{1}, compIdxOut);
  dataStats = cellFlatMap(@(x) x{2}, compIdxOut);
  dataLines = filter1d(@(l) numel(l) > 0, dataLines);
  dataStats = filter1d(@(s) numel(s) > 0, dataStats);
  dataOut = strjoin(dataLines, '\n');
  header = 'Met, CompIdx, InfluxIdx, OutfluxIdx, FluxInfo';
  dataOut = strjoin({header, dataOut}, '\n');

  % iKeys = cellFlatMap(@(x) num2str(x), num2cell(1:nSpecies));
  [repOLapStruct{1:nSpecies}] = deal(struct('count', 0, 'list', []));
  for ii = 1:nSpecies
    rs = repOLapStruct{ii};
    rs.org = model.infoCom.spAbbr{ii};
    repOLapStruct{ii} = rs;
  end
  overlappingInputs = containers.Map(model.infoCom.spAbbr, repOLapStruct);

  for sIx = 1:numel(dataStats)
    % inFluxIx has position 2, but check this in competitionIndex.m
    % same for inOrgs being position 4
    ss = dataStats{sIx};
    inFluxIx = str2num(ss{2});
    inOrgs = ss{4};
    assert(inFluxIx == numel(inOrgs));
    for oIx = 1:inFluxIx
      if inFluxIx > 1
        org = inOrgs{oIx};
        orgStats = overlappingInputs(org);
        orgStats.count = orgStats.count + 1;
        orgStats.list = [orgStats.list (inFluxIx-1)];
        overlappingInputs(org) = orgStats;
      end
    end
  end
end

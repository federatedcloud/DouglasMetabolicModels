% For a given flux vector, from a multi-species model, determines the competition
% index for each reacion, the interpretation of which is dependent on the number
% of species present in the model.
function [overlappingTr, overlappingTrNoInorg] = competitionIndices(model, flux)
  rxns = model.rxns;
  nSpecies = numel(model.infoCom.spAbbr);

  exRxnGroups = readExRxnGroups();
  inorgExRxnPrefixes = exRxnGroups(inorganicIonGroupName());
  inorgExRxns = catSpeciesRxnPrefixes(inorgExRxnPrefixes, model);

  % transRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^[A-Z]{2}IEX.*tr$')));
  exRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^EX.*\[u\]$')));
  exRxnsNoInorg = setdiff(exRxns, inorgExRxns);
  overlappingTr = cmpIndInner(exRxns);
  overlappingTrNoInorg = cmpIndInner(exRxnsNoInorg);

  function cmpIndInner(exRxnSet)
    compIdxOut = cellFlatMap(@(r) competitionIndex(r, model, flux), exRxnSet);
    dataLines = cellFlatMap(@(x) x{1}, compIdxOut);
    dataStats = cellFlatMap(@(x) x{2}, compIdxOut);
    dataLines = filter1d(@(l) numel(l) > 0, dataLines);
    dataStats = filter1d(@(s) numel(s) > 0, dataStats);
    % dataOut = strjoin(dataLines, '\n');
    % header = 'Met, CompIdx, InfluxIdx, OutfluxIdx, FluxInfo';
    % dataOut = strjoin({header, dataOut}, '\n');

    % iKeys = cellFlatMap(@(x) num2str(x), num2cell(1:nSpecies));
    [repOLapStruct{1:nSpecies}] = deal(struct( ...
      'countIn', 0, 'listIn', [], 'countOut', 0, 'listOut', [] ...
    ));
    for ii = 1:nSpecies
      rs = repOLapStruct{ii};
      rs.org = model.infoCom.spAbbr{ii};
      repOLapStruct{ii} = rs;
    end
    overlappingTr = containers.Map(model.infoCom.spAbbr, repOLapStruct);
    overlappingTr = addOrgStats(overlappingTr);

    function overlapping = addOrgStats(overlapping)
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
            orgStats = overlapping(org);
            orgStats.countIn = orgStats.countIn + 1;
            orgStats.listIn = [orgStats.listIn (inFluxIx-1)];
            overlapping(org) = orgStats;
          end % if inFluxIx
        end % of for oIx
        outFluxIx = str2num(ss{3});
        outOrgs = ss{5};
        assert(outFluxIx == numel(outOrgs));
        for oIx = 1:outFluxIx
          if outFluxIx > 1
            org = outOrgs{oIx};
            orgStats = overlapping(org);
            orgStats.countOut = orgStats.countOut + 1;
            orgStats.listOut = [orgStats.listOut (outFluxIx-1)];
            overlapping(org) = orgStats;
          end % if outFluxIx
        end % of for oIx
      end % of for sIx
    end % of function addOrgStats
  end

end

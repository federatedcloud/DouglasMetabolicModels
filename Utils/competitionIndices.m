% For a given flux vector, from a multi-species model, determines the competition
% index for each reacion, the interpretation of which is dependent on the number
% of species present in the model.
function [overlappingTr, overlappingTrNoInorg] = competitionIndices(model, flux)
  rxns = model.rxns;
  nSpecies = numel(model.infoCom.spAbbr);

  fluxThreshold = 0;
  fluxIsIn = flux < -1 * fluxThreshold;
  fluxIsOut = flux > fluxThreshold;

  exRxnGroups = readExRxnGroups();
  inorgExRxnPrefixes = exRxnGroups(inorganicIonGroupName());
  inorgExRxns = cellFlatMap(@(r) strrep(r, '_e', '[u]'), inorgExRxnPrefixes);

  trRxnGroups = readTrRxnGroups();
  inorgTrRxnsPrefixes = trRxnGroups(inorganicIonGroupName());
  inorgTrRxns = model.rxns(catSpeciesRxnPrefixes(inorgTrRxnsPrefixes, model));

  % transRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^[A-Z]{2}IEX.*tr$')));
  exRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, '^EX.*\[u\]$')));
  trRxns = findTrRxns(model);
  exRxnsNoInorg = setdiff(exRxns, inorgExRxns);
  trRxnsNoInorg = setdiff(trRxns, inorgTrRxns);
  overlappingTr = cmpIndInner(exRxns, trRxns);
  overlappingTrNoInorg = cmpIndInner(exRxnsNoInorg, trRxnsNoInorg);

  function overlappingTr = cmpIndInner(exRxnSet, trRxnSet)
    exRxnSetIxs = find(contains(model.rxns, exRxnSet));
    compIdxOut = cellFlatMap(@(r) competitionIndex(r, model, flux), exRxnSet);
    dataLines = cellFlatMap(@(x) x{1}, compIdxOut);
    dataLines = filter1d(@(l) numel(l) > 0, dataLines);
    dataStats = cellFlatMap(@(x) x{2}, compIdxOut);
    dataStats = filter1d(@(s) numel(s) > 0, dataStats);
    % dataOut = strjoin(dataLines, '\n');
    % header = 'Met, CompIdx, InfluxIdx, OutfluxIdx, FluxInfo';
    % dataOut = strjoin({header, dataOut}, '\n');

    % iKeys = cellFlatMap(@(x) num2str(x), num2cell(1:nSpecies));
    [repOLapStruct{1:nSpecies}] = deal(struct(                              ...
      'countIn', 0, 'listIn', [], 'countOut', 0, 'listOut', [],             ...
      'inFluxSum', 0, 'outFluxSum', 0, 'inFluxCount', 0, 'outFluxCount', 0, ...
      'inRxnsOverlap', {{}}, 'outRxnsOverlap', {{}}, 'org', struct(struct())    ...
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
        inFluxIx = str2double(ss{2});
        inOrgs = ss{4};
        inRxns = ss{8};
        assert(inFluxIx == numel(inOrgs));
        for oIx = 1:inFluxIx
          org = inOrgs{oIx};
          orgStats = overlapping(org);
          orgStats.inFluxSum = orgStats.inFluxSum + ss{6};
          if inFluxIx > 1
            orgStats.countIn = orgStats.countIn + 1;
            orgStats.listIn = [orgStats.listIn (inFluxIx-1)];
            orgStats.inRxnsOverlap{end+1} = inRxns(1);
            overlapping(org) = orgStats;
          end % if inFluxIx
        end % of for oIx
        outFluxIx = str2double(ss{3});
        outOrgs = ss{5};
        outRxns = ss{9};
        assert(outFluxIx == numel(outOrgs));
        for oIx = 1:outFluxIx
          org = outOrgs{oIx};
          orgStats = overlapping(org);
          orgStats.outFluxSum = orgStats.outFluxSum + ss{7};
          if outFluxIx > 1
            orgStats.countOut = orgStats.countOut + 1;
            orgStats.listOut = [orgStats.listOut (outFluxIx-1)];
            orgStats.outRxnsOverlap{end+1} = outRxns(1);
            overlapping(org) = orgStats;
          end % if outFluxIx
        end % of for oIx
      end % of for sIx
      for ii = 1:nSpecies
        org = model.infoCom.spAbbr{ii};
        orgTrRxns = filter1d(@(r) startsWith(r, org), trRxnSet);
        orgTrRxnIxs = contains(model.rxns, orgTrRxns);
        inFluxTrIxs = fluxIsIn & orgTrRxnIxs;
        outFluxTrIxs = fluxIsOut & orgTrRxnIxs ;
        orgStats = overlapping(org);
        orgStats.inFluxCount = sum(inFluxTrIxs);
        orgStats.outFluxCount = sum(outFluxTrIxs);
        orgStats.inFluxRxns = model.rxns(inFluxTrIxs);
        orgStats.outFluxRxns = model.rxns(outFluxTrIxs);
        orgStats.inRxnsOverlap = cellFlatMap(@(x) x{1}, orgStats.inRxnsOverlap);
        orgStats.outRxnsOverlap = cellFlatMap(@(x) x{1}, orgStats.outRxnsOverlap);
        overlapping(org) = orgStats;
      end  % end of for ii = 1:nSpecies
    end % of function addOrgStats
  end

end

% Generates a table for use in creating a flux heatmap:
% Row headers are reaction names.
% Column headers are organism concatenated with community.
%  - There is an additional single column header for reaction group
%
% Only transport reactions and efflux-exchange reactions are considered.
%
function tables = genHeatMapTables(analysis)
  allSpecies = {'AF', 'AP', 'AT', 'LB', 'LP'};
  nOrgsTotal = numel(allSpecies);

  trRxnGroups = readTrRxnGroups();
  exRxnGroups = readExRxnGroups();
  combinedKeys = union(keys(trRxnGroups), keys(exRxnGroups));
  combinedRxnGroup = containers.Map();
  modelMap = containers.Map();
  resultMap = containers.Map();
  for ci = 1:numel(combinedKeys)
    gKey = combinedKeys{ci};
    combinedRxnGroup(gKey) = ...
      union(getOrEmpty(@() trRxnGroups(gKey)),  getOrEmpty(@() exRxnGroups(gKey)));
  end
  trRxnGroupsNoInorgIons = containers.Map(trRxnGroups.keys, trRxnGroups.values);
  exRxnGroupsNoInorgIons = containers.Map(exRxnGroups.keys, exRxnGroups.values);
  trRxnGroupsNoInorgIons.remove(inorganicIonGroupName());
  exRxnGroupsNoInorgIons.remove(inorganicIonGroupName());

  tables = containers.Map();
  tables('trans') = genHMTable(trRxnGroups, true);
  tables('exchange') = genHMTable(exRxnGroups, false);
  tables('trans_noInorgIons') = genHMTable(trRxnGroupsNoInorgIons, true);
  tables('exchange_noInorgIons') = genHMTable(exRxnGroupsNoInorgIons, false);

  function cellTbl = genHMTable(rxnGroups, isTrans)


    nFluxes = countRxnsInIdMap(rxnGroups);

    groupNames = keys(rxnGroups);
    groupHeaders = cell(nFluxes + nOrgsTotal + 1, 1); % fluxes + BM + GRmax
    rxnHeaders = cell(nFluxes + nOrgsTotal + 1, 1); % fluxes + BM + GRmax
    rowPos = 0;
    for ii = 1:numel(groupNames)
      groupName = groupNames{ii};
      rxnsInGrp = rxnGroups(groupName);
      for jj = 1:numel(rxnsInGrp)
        rowPos = rowPos + 1;
        rxnId = rxnsInGrp{jj};
        groupHeaders(rowPos) = {groupName};
        rxnHeaders(rowPos) = {rxnId};
      end
    end
    rxnHeaders(end-nOrgsTotal:end-1) = ...
      cellFlatMap(@(s) strjoin({'Biomass_', s}, ''), allSpecies);
    groupHeaders(end-nOrgsTotal:end-1) = deal({'Biomass'});
    rxnHeaders(end) = {'CommunityGrowth'};
    groupHeaders(end) = {'CommunityGrowth'};
    function excRxnIds = excMultiSub(rxnIds, multiModel)
      rxnIdsMMT = union( ...
        cellFlatMap(@(r) regexprep(r, '_e$', '[u]'), rxnIds), ...
        multiModel.rxns(findBiomassRxnIds(multiModel)) ...
      );
      rxnIxsMMT = cell2mat(cellFlatMap(@(r) find(strcmp(r, multiModel.rxns)), rxnIdsMMT));
      rxnIxsMM = filter1d(@(r) r > 0, rxnIxsMMT);
      excRxnIds = cellFlatMap(@(r) cellHead(multiModel.rxns(r)), num2cell(rxnIxsMM));
    end

    % Note: currently most likely unused, consider removal
    function commName = commPart(orgCommKey)
      splitKey = split(orgCommKey, ":");
      commName = splitKey{2};
    end

    function orgName = orgPart(orgCommKey)
      splitKey = split(orgCommKey, ":");
      orgName = splitKey{1};
    end

    function numRxns = countRxnsInIdMap(idMap)
      numRxns = 0;
      mVals = values(idMap);
      for zi = 1:numel(mVals)
        iiRxns = mVals{zi};
        numRxns = numRxns + numel(iiRxns);
      end
    end

    function tnmap = makeNormRxnMap(multiModel)
      tnmap = containers.Map();
      for ri = 1:numel(multiModel.rxns)
        rId = multiModel.rxns{ri};
        choppedRxn = extractAfterMulti(rId, multiModel.infoCom.spAbbr);
        exSCRxn = strrep(rId, '[u]', '_e');
        tentativeRxns = {rId, choppedRxn, exSCRxn};
        canonIxs = [];
        for ti = 1:numel(tentativeRxns)
          cIxMaybe = find(strcmp(tentativeRxns{ti}, rxnHeaders));
          if cIxMaybe > 0
            canonIxs(end+1) = cIxMaybe;
          end
        end
        for rci = 1:numel(canonIxs)
          cIx = canonIxs(rci);
          if contains(rId, 'O2tex')
            disp({'BEG', rId, rxnHeaders{cIx}, 'END'}); % FIXME
          end
          tnmap(rId) = rxnHeaders{cIx};
        end
      end
      % tnmap_keys = keys(tnmap) % FIXME
      % tnmap_values = values(tnmap) % FIXME
    end

    % Maps rxn idx to row set
    function newIxMap = rxnIxToRowIx(rxnIxs, multiModel)
      normMap =  makeNormRxnMap(multiModel);
      newIxMap = containers.Map('KeyType', 'double', 'ValueType', 'any');
      for zi = 1:numel(rxnIxs)
        ixOrig = rxnIxs(zi);
        rId = multiModel.rxns{ixOrig};
        rowRxn = '';
        try % FIXME
          rowRxn = normMap(rId);
        catch
          disp(strjoin({'Rxn not found: ', rId}, ''));
        end
        ixMatches = find(strcmp(rowRxn, rxnHeaders));
        newIxMap(ixOrig) = ixMatches;
      end
    end

    fluxMap = containers.Map();
    rxnMap = containers.Map(); % FIXME: Do we need this?
    rxnIdMap = containers.Map();
    rxnIxMap = containers.Map();

    fvalsMap = containers.Map();
    for simIx = 1:numel(analysis.fvalues);
      fVal = analysis.fvalues{simIx};
      commKey = commString(fVal.model);
      fvalsMap(commKey) = fVal;
    end
    fValsUniq = values(fvalsMap);

    commGroupMap = containers.Map();
    for simIx = 1:numel(fValsUniq)
      afVal = fValsUniq{simIx};
      nOrgs = numel(afVal.model.infoCom.spAbbr);
      commStr = commString(afVal.model);
      modelMap(commStr) = afVal.model;
      resultMap(commStr) = afVal.res;
      orgCommKeys = cell(1, nOrgs);
      for ii = 1:nOrgs
        org = afVal.model.infoCom.spAbbr{ii};
        orgCommKeys{ii} = strjoin({org, commStr}, ':');

        groups = keys(rxnGroups);
        nGroups = numel(groups);
        rxnIxs = [];
        for jj = 1:nGroups
          group = groups{jj};
          gRxns = rxnGroups(group);
          if isTrans
            gSpRxns = catSpeciesRxnPrefixes(gRxns, afVal.model, org);
            rxnIxs = [rxnIxs(:)' gSpRxns(:)'];
          else
            gRxnsMM = excMultiSub(gRxns, afVal.model);
            gRxnIds = filter1d(@(r) r > 0, findRxnIDs(afVal.model, gRxnsMM));
            rxnIxs = [rxnIxs(:)' gRxnIds(:)'];
          end
        end
        fluxMap(orgCommKeys{ii}) = afVal.res.flux(rxnIxs);
        rxnMap(orgCommKeys{ii}) = afVal.model.rxnNames(rxnIxs);
        rxnIdMap(orgCommKeys{ii}) = afVal.model.rxns(rxnIxs);
        rxnIxMap(orgCommKeys{ii}) = rxnIxs;
      end
      commGroupMap(commStr) = orgCommKeys;
    end % of for ii = 1:nOrgs

    nComOrgKeys = numel(keys(fluxMap));

    nRows = numel(rxnHeaders) + 2; % + community label + org label
    nCols = nComOrgKeys + 2; % + group label + rxn label

    cellTbl = cell(nRows, nCols);
    comms = sortByColFun({@(x) numel(x), @(x) x}, [1, 1], keys(commGroupMap));
    nComs = numel(comms);
    fluxPos = 2; % Header columns occupy cols 1 & 2, so start below at 3.
    for ii = 1:nComs
      comm = comms{ii};
      orgCommKeys = sortByColFun( ...
        {@(x) numel(orgPart(x)), @(x) orgPart(x)}, [1, 1], commGroupMap(comm));
      nOrgs = numel(orgCommKeys);
      for jj = 1:nOrgs
        fluxPos = fluxPos + 1;
        orgComKey = orgCommKeys{jj};
        cellTbl(1, fluxPos) = {comm};
        cellTbl(2, fluxPos) = {orgPart(orgComKey)};
        fluxes = fluxMap(orgComKey);
        rxnIxs = rxnIxMap(orgComKey);
        rowIxMap = rxnIxToRowIx(rxnIxs, modelMap(comm));
        for kk = 1:numel(fluxes)
          rxnIx = rxnIxs(kk);
          rows = rowIxMap(rxnIx);
          cellTbl(2 + rows, fluxPos) = num2cell(fluxes(kk));
        end
        cellTbl(end-nOrgsTotal:end-1, fluxPos) = ...
          makeFullBMvec(resultMap(comm).BM, modelMap(comm));
        cellTbl(end, fluxPos) = num2cell(resultMap(comm).GRmax);
      end
    end % of for ii = 1:nComs

    cellTbl(3:end, 1) = groupHeaders;
    cellTbl(3:end, 2) = rxnHeaders;

    assert(all(size(cellTbl) == [nRows nCols]));
  end % of genHMTable

  function fullCell = makeFullBMvec(inVec, model)
    fullCell = cell(nOrgsTotal, 1);
    nSpecies = numel(model.infoCom.spAbbr);
    for ii = 1:nSpecies
      org = model.infoCom.spAbbr{ii};
      orgIx = find(strcmp(allSpecies, org));
      fullCell(orgIx) = num2cell(inVec(ii));
    end
  end

end

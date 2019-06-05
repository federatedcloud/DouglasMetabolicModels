% Generates a table for use in creating a flux heatmap:
% Row headers are reaction names.
% Column headers are organism concatenated with community.
%  - There is an additional single column header for reaction group
%
% Only transport reactions and efflux-exchange reactions are considered.
%
function tables = genHeatMapTables(analysis)
  trRxnGroups = readRxnGroups('../../models/trans_reaction.groups_052919.csv');
  exRxnGroups = readRxnGroups('../../models/ex_reaction.groups_052919.csv');
  combinedKeys = union(keys(trRxnGroups), keys(exRxnGroups));
  combinedRxnGroup = containers.Map();
  modelMap = containers.Map();
  for ci = 1:numel(combinedKeys)
    gKey = combinedKeys{ci};
    combinedRxnGroup(gKey) = ...
      union(getOrEmpty(@() trRxnGroups(gKey)),  getOrEmpty(@() exRxnGroups(gKey)));
  end

  tables.trans = genHMTable(trRxnGroups, true);
  tables.exchange = genHMTable(exRxnGroups, false); % FIXME

  function cellTbl = genHMTable(rxnGroups, isTrans)

    nFluxes = countRxnsInIdMap(rxnGroups);

    groupNames = keys(rxnGroups);
    groupHeaders = cell(nFluxes, 1);
    rxnHeaders = cell(nFluxes, 1);
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

    function trRxnsFnd = trCatRxns(rxns, org, multiModel)
      tentativeRxns = cellFlatMap(@(r) strjoin({org, r}, ''), rxns);
      trRxnsFnd = filter1d(@(r) r > 0, findRxnIDs(multiModel, tentativeRxns));
    end

    function excRxnIds = excMultiSub(rxnIds, multiModel)
      rxnIdsMMT = cellFlatMap(@(r) strrep(r, '(e)', '[u]'), rxnIds);
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
        exSCRxn = strrep(rId, '[u]', '(e)');
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

    commGroupMap = containers.Map();
    for simIx = 1:numel(analysis.fvalues)
      afVal = analysis.fvalues{simIx};
      nOrgs = numel(afVal.model.infoCom.spAbbr);
      commStr = commString(afVal.model);
      modelMap(commStr) = afVal.model;
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
            gSpRxns = trCatRxns(gRxns, org, afVal.model);
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

    nRows = nFluxes + 2; % + community label + org label
    nCols = nComOrgKeys + 2; % + group label + rxn label

    cellTbl = cell(nRows, nCols);
    comms = sortByColFun({@(x) numel(x), @(x) x}, [1, 1], keys(commGroupMap));
    nComs = numel(comms);
    fluxPos = 2; % Header columns occupy cols 1 & 2, so start below at 3.
    max_row_ix = 0; % FIXME
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
        max_row_ix_tmp = max(cell2mat(values(rowIxMap))); % FIXME
        if max_row_ix_tmp > max_row_ix % FIXME
          max_row_ix = max_row_ix_tmp;  % FIXME
        end  % FIXME
        for kk = 1:numel(fluxes)
          rxnIx = rxnIxs(kk);
          rows = rowIxMap(rxnIx);
          cellTbl(2 + rows, fluxPos) = num2cell(fluxes(kk));
        end
        % return; % FIXME
      end
    end % of for ii = 1:nComs
    max_row_ix = max_row_ix % FIXME

    cellTbl(3:end, 1) = groupHeaders;
    cellTbl(3:end, 2) = rxnHeaders;

    assert(all(size(cellTbl) == [nRows nCols]));
  end % of genHMTable

end

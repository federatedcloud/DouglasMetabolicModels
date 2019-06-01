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
  for ii = 1:numel(combinedKeys)
    gKey = combinedKeys{ii};
    combinedRxnGroup(gKey) = ...
      union(getOrEmpty(@() trRxnGroups(gKey)),  getOrEmpty(@() exRxnGroups(gKey)));
  end

  tables.trans = genHMTable(trRxnGroups, true);
  tables.exchange = genHMTable(exRxnGroups, false);

  function cellTbl = genHMTable(rxnGroups, isTrans)

    function trRxnsFnd = trCatRxns(rxns, org, multiModel)
      nRxns = numel(rxns);
      tentativeRxns = cellFlatMap(@(r) strjoin({'AF', r}, ''), rxns);
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
      for ii = 1:numel(mVals)
        iiRxns = mVals{ii};
        numRxns = numRxns + numel(iiRxns);
      end
    end

    function newIxMap = rxnIxToRowIx(rIxMap)
      ixSets = values(rIxMap);
      allIx = [];
      for ii = 1:numel(ixSets)
        ixSet = ixSets{ii};
        allIx = union(allIx, ixSet);
      end

      newIxMap = rIxMap;
      gKeys = keys(newIxMap);
      for ii = 1:numel(gKeys)
        gKey = gKeys{ii};
        ixSet = newIxMap(gKey);
        for jj = 1:numel(ixSet)
          ix = ixSet(jj);
          ixSet(jj) = find(ix == allIx);
        end
        newIxMap(gKey) = ixSet;
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

    nComOrgKeys = numel(keys(fluxMap))

    nFluxes = countRxnsInIdMap(rxnGroups)
    nRows = nFluxes + 2; % + community label + org label
    nCols = nComOrgKeys + 2; % + group label + rxn label

    cellTbl = cell(nRows, nCols);
    comms = sortByColFun({@(x) numel(x), @(x) x}, [1, 1], keys(commGroupMap));
    nComs = numel(comms);
    fluxPos = 2; % Header columns occupy cols 1 & 2, so start below at 3.
    rowIxMap = rxnIxToRowIx(rxnIxMap);
    for ii = 1:nComs
      comm = comms{ii};
      orgCommKeys = sortByColFun( ...
        {@(x) numel(orgPart(x)), @(x) orgPart(x)}, [1, 1], commGroupMap(comm));
      nOrgs = numel(orgCommKeys);
      for jj = 1:nOrgs
        fluxPos = fluxPos + 1;
        orgComKey = orgCommKeys{jj};
        max(rowIxMap(orgComKey)) % FIXME
        cellTbl(rowIxMap(orgComKey), fluxPos) = num2cell(fluxMap(orgComKey));
      end
    end % of for ii = 1:nComs
  end % of genHMTable

end

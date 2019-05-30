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
  combinedKeys = untion(keys(trRxnGroups), keys(exRxnGroups));
  combinedRxnGroup = containers.Map();
  for ii = 1:numel(combinedKeys)
    gKey = combinedKeys{ii};
    combinedRxnGroup(gKey) = union(trRxnGroups(gKey),  exRxnGroups(gKey));
  end

  ## function trRxnsFnd = trCatRxns(rxn, multiModel)
  ##   nOrgs = numel(afVal.model.infoCom.spAbbr);
  ##   tentativeRxns = cell(1, nOrgs);
  ##   for ii = 1:nOrgs
  ##     org = afVal.model.infoCom.spAbbr{ii};
  ##     tentativeRxns{ii} = strjoin({org, rxn}, '');
  ##     fluxMap(orgCommKey) = afVal.res.flux;
  ##   end
  ##   trRxnsFnd = filter1d(@(r) r > 0, findRxnIDs(multiModel, tentativeRxns));
  ## end

  function trRxnsFnd = trCatRxns(rxns, org, multiModel)
    nRxns = numel(rxns);
    tentativeRxns = cellFlatMap(@(r) strjoin({'AF', r}, ''), rxns);
    trRxnsFnd = filter1d(@(r) r > 0, findRxnIDs(multiModel, tentativeRxns));
  end


  function nestedRxnMap = trueRxnMap(rxnGroup, isTrans)
    nestedRxnMap = containers.Map();
    groups = keys(rxnGroup);
    nGroups = numel(groups);
    for ii = 1:nGroups
      gRxns = rxnGroup(groups(ii));
      for jj = 1:numel(gRxns)
        rxn = gRxns{ii};
        if isTrans
          % FIXME
        else
          nestedRxnMap(groups(ii)) = {rxn};
        end
      end
    end
  end

  function tblOut = genHMTable(rxnGroup, isTrans)

    fluxMap = containers.Map();
    rxnMap = containers.Map();

    commGroupMap = containers.Map();
    for simIx = 1:numel(analysis.fvalues)
      afVal = analysis.fvalues{simIx};
      effluxRxns = findEffluxRxns(afVal.model);
      trRxns = findTrRxns(afVal.model);

      nOrgs = numel(afVal.model.infoCom.spAbbr);
      commStr = commString(afVal.model);
      orgCommKeys = cell(1, nOrgs);
      for ii = 1:nOrgs
        org = afVal.model.infoCom.spAbbr{ii};
        orgCommKeys{ii} = strjoin({org, commStr}, ':');

        groups = keys(rxnGroup);
        nGroups = numel(groups);
        rxnIds = [];
        for jj = 1:nGroups
          group = groups{jj};
          gRxns = rxnGroup(group);
          if isTrans
            gSpRxns = trCatRxns(gRxns, org, afVal.model);
            rxnIds = [rxnIds(:)' gSpRxns(:)'];
          else
            gRxnIds = filter1d(@(r) r > 0, findRxnIDs(afVal.model, gRxns))
            rxnIds = [rxnIds(:)' gRxnIds(:)'];
          end
        end
        fluxMap(orgCommKeys{ii}) = afVal.res.flux(rxnIds);
        rxnMap(orgCommKeys{ii}) = afVal.model.rxnNames(rxnIds);
      end
      commGroupMap(commStr) = orgCommKeys;
      nComOrgKeys = numel(keys(fluxMap));
      nFluxes = sum(cell2mat(cellFlatMap(@(x) numel(x), values(fluxMap))));
      nRows = nFluxes + 2; % + community label + org label
      nCols = nComOrgKeys + 2; % + group label + rxn label

      cellTbl = cell(nRows, nCols);
      for 
      end


    end


  end

end

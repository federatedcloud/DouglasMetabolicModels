function [CMP, tbl] = cmpAnalysis(childRes, parentRes, noInorgIons, setDEBUG)
  CMP = struct;

  childName = commString(childRes.model);
  parName = commString(parentRes.model);

  fluxActThresh = 1e-6;
  childRFmap = containers.Map(childRes.model.rxns, childRes.res.flux);
  parRFmap = containers.Map(parentRes.model.rxns, parentRes.res.flux);
  rxnsDiff = {};
  childRxnFlux = {};
  parRxnFlux = {};
  parentGainedRxns = {};
  childLostRxns = {};

  excRxns = parentRes.model.rxns(findExcIDs(parentRes.model));
  trRxns = findTrRxns(parentRes.model);

  DEBUG = false;
  if (nargin > 3)
    DEBUG = setDEBUG;
  end

  % really we should just be able to use the smaller reaction set, as
  % as it should be a subset of the parent reaction set, so no need for this:
  commonRxns = intersect(childRes.model.rxns, parentRes.model.rxns);
  if noInorgIons
    exRxnGroups = readExRxnGroups();
    inorgExRxnPrefixes = exRxnGroups(inorganicIonGroupName());
    inorgExRxns = cellFlatMap(@(r) strrep(r, '_e', '[u]'), inorgExRxnPrefixes);
    trRxnGroups = readTrRxnGroups();
    inorgTrRxnPrefixes = trRxnGroups(inorganicIonGroupName());
    inorgTrRxnIxs = catSpeciesRxnPrefixes(inorgTrRxnPrefixes, parentRes.model);
    inorgTrRxns = parentRes.model.rxns(inorgTrRxnIxs);
    commonRxns = setdiff(commonRxns, union(inorgExRxns, inorgTrRxns));
  end
  childActiveCount = 0;
  parentActiveCount = 0;
  childActiveRxns = {};
  parentActiveRxns = {};

  childActiveInExCount = 0;
  parentActiveInExCount = 0;
  childActiveInTrCount = 0;
  parentActiveInTrCount = 0;
  childActiveInExRxns = {};
  parentActiveInExRxns = {};
  childActiveInTrRxns = {};
  parentActiveInTrRxns = {};

  childActiveOutExCount = 0;
  parentActiveOutExCount = 0;
  childActiveOutTrCount = 0;
  parentActiveOutTrCount = 0;
  childActiveOutExRxns = {};
  parentActiveOutExRxns = {};
  childActiveOutTrRxns = {};
  parentActiveOutTrRxns = {};

  if DEBUG
    trRxnFID = fopen('trRxnFluxes.csv', 'w');
  end
  for ii = 1:numel(commonRxns)
    rxn = char(commonRxns{ii});
    if sign(childRFmap(rxn)) ~= sign(parRFmap(rxn))
      rxnsDiff{end+1} = rxn;
      childRxnFlux{end+1} = childRFmap(rxn);
      parRxnFlux{end+1} = parRFmap(rxn);
    end
    % Measure activity for Community Metabolic Potential (CMP)
    childIsAct = 0;
    parIsAct = 0;
    if abs(childRFmap(rxn)) > fluxActThresh
      childActiveCount = childActiveCount + 1;
      childActiveRxns{end+1} = rxn;
      childIsAct = 1;
      if any(strcmp(rxn, excRxns))
        if sign(childRFmap(rxn)) > 0
          childActiveOutExCount = childActiveOutExCount + 1;
          childActiveOutExRxns{end+1} = rxn;
        elseif sign(childRFmap(rxn)) < 0
          childActiveInExCount = childActiveInExCount + 1;
          childActiveInExRxns{end+1} = rxn;
        end
      elseif any(strcmp(rxn, trRxns))
        if sign(childRFmap(rxn)) > 0
          childActiveOutTrCount = childActiveOutTrCount + 1;
          childActiveOutTrRxns{end+1} = rxn;
        elseif sign(childRFmap(rxn)) < 0
          childActiveInTrCount = childActiveInTrCount + 1;
          childActiveInTrRxns{end+1} = rxn;
        end
        if DEBUG
          fprintf(trRxnFID, '%s\t%d\n', rxn, childRFmap(rxn));
        end
      end
    end
    if abs(parRFmap(rxn)) > fluxActThresh
      parentActiveCount = parentActiveCount + 1;
      parentActiveRxns{end+1} = rxn;
      parIsAct = 1;
      if any(strcmp(rxn, excRxns))
        if sign(parRFmap(rxn)) > 0
          parentActiveOutExCount = parentActiveOutExCount + 1;
          parentActiveOutExRxns{end+1} = rxn;
        elseif sign(parRFmap(rxn)) < 0
          parentActiveInExCount = parentActiveInExCount + 1;
          parentActiveInExRxns{end+1} = rxn;
        end
      elseif any(strcmp(rxn, trRxns))
        if sign(parRFmap(rxn)) > 0
          parentActiveOutTrCount = parentActiveOutTrCount + 1;
          parentActiveOutTrRxns{end+1} = rxn;
        elseif sign(parRFmap(rxn)) < 0
          parentActiveInTrCount = parentActiveInTrCount + 1;
          parentActiveInTrRxns{end+1} = rxn;
        end
      end
    end
    if (parIsAct > childIsAct)
      parentGainedRxns{end+1} = rxn;
    elseif (childIsAct > parIsAct)
      childLostRxns{end+1} = rxn;
    end
  end

  % Probably no need for a non-inorganic-ion table here:
  tbl = makeTable(rxnsDiff, childRxnFlux, parRxnFlux);
  if ~noInorgIons
    parNameTbl = parName;
    if strcmp(parName, childName)
      parNameTbl = strjoin({parName, '_'}, '');
    end
    tbl.Properties.VariableNames = {'Reaction', childName, parNameTbl};
  end

  CMP.child = childActiveCount;
  CMP.parent = parentActiveCount;

  CMP.InExChild = childActiveInExCount;
  CMP.InExParent = parentActiveInExCount;
  CMP.InTrChild = childActiveInTrCount;
  CMP.InTrParent = parentActiveInTrCount;

  CMP.OutExChild = childActiveOutExCount;
  CMP.OutExParent = parentActiveOutExCount;
  CMP.OutTrChild = childActiveOutTrCount;
  CMP.OutTrParent = parentActiveOutTrCount;

  CMP.UniqChild = ...
    numel(flattenRxnsAcrossSpecies(childActiveRxns, childRes.model));
  CMP.UniqParent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveRxns, parentRes.model));

  CMP.InExUniqChild = ...
    numel(flattenRxnsAcrossSpecies(childActiveInExRxns, childRes.model));
  CMP.InExUniqParent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveInExRxns, parentRes.model));
  CMP.InTrUniqChild = ...
    numel(flattenRxnsAcrossSpecies(childActiveInTrRxns, childRes.model));
  CMP.InTrUniqParent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveInTrRxns, parentRes.model));

  CMP.OutExUniqChild = ...
    numel(flattenRxnsAcrossSpecies(childActiveOutExRxns, childRes.model));
  CMP.OutExUniqParent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveOutExRxns, parentRes.model));
  CMP.OutTrUniqChild = ...
    numel(flattenRxnsAcrossSpecies(childActiveOutTrRxns, childRes.model));
  CMP.OutTrUniqParent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveOutTrRxns, parentRes.model));

  CMP.parentGainedRxns = parentGainedRxns;
  CMP.childLostRxns = childLostRxns;

end

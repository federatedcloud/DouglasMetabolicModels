%
% childRes  In this sense has a flux derived from a model with
%           one fewer species than the parentRes.
% parentRes See childRes.
%
function pairAnalysis = compareFluxes(childRes, parentRes, setDEBUG)
  fluxActThresh = 1e-6;
  childRFmap = containers.Map(childRes.model.rxns, childRes.res.flux);
  parRFmap = containers.Map(parentRes.model.rxns, parentRes.res.flux);
  rxnsDiff = {};
  childRxnFlux = {};
  parRxnFlux = {};
  parentGainedRxns = {};
  childLostRxns = {};
  pairAnalysis = struct;

  excRxns = parentRes.model.rxns(findExcIDs(parentRes.model));
  trRxns = findTrRxns(parentRes.model);

  DEBUG = false;
  if (nargin > 2)
    DEBUG = setDEBUG;
  end

  % really we should just be able to use the smaller reaction set, as
  % as it should be a subset of the parent reaction set, so no need for this:
  commonRxns = intersect(childRes.model.rxns, parentRes.model.rxns);
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
  tbl = makeTable(rxnsDiff, childRxnFlux, parRxnFlux);
  childName = commString(childRes.model);
  parName = commString(parentRes.model);
  pairAnalysis.size.child = numel(childRes.model.infoCom.spAbbr);
  pairAnalysis.size.parent = numel(parentRes.model.infoCom.spAbbr);
  parNameTbl = parName;
  if strcmp(parName, childName)
    parNameTbl = strjoin({parName, '_'}, '');
  end
  tbl.Properties.VariableNames = {'Reaction', childName, parNameTbl};
  pairAnalysis.childName = childName;
  pairAnalysis.parName = parName;
  pairAnalysis.label = strjoin({childName, '__vs__', parName}, '');
  pairAnalysis.table = tbl;
  pairAnalysis.cmp.child = childActiveCount;
  pairAnalysis.cmp.parent = parentActiveCount;

  pairAnalysis.cmpInEx.child = childActiveInExCount;
  pairAnalysis.cmpInEx.parent = parentActiveInExCount;
  pairAnalysis.cmpInTr.child = childActiveInTrCount;
  pairAnalysis.cmpInTr.parent = parentActiveInTrCount;

  pairAnalysis.cmpOutEx.child = childActiveOutExCount;
  pairAnalysis.cmpOutEx.parent = parentActiveOutExCount;
  pairAnalysis.cmpOutTr.child = childActiveOutTrCount;
  pairAnalysis.cmpOutTr.parent = parentActiveOutTrCount;

  pairAnalysis.cmpUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveRxns, childRes.model));
  pairAnalysis.cmpUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveRxns, parentRes.model));

  pairAnalysis.cmpInExUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveInExRxns, childRes.model));
  pairAnalysis.cmpInExUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveInExRxns, parentRes.model));
  pairAnalysis.cmpInTrUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveInTrRxns, childRes.model));
  pairAnalysis.cmpInTrUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveInTrRxns, parentRes.model));

  pairAnalysis.cmpOutExUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveOutExRxns, childRes.model));
  pairAnalysis.cmpOutExUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveOutExRxns, parentRes.model));
  pairAnalysis.cmpOutTrUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveOutTrRxns, childRes.model));
  pairAnalysis.cmpOutTrUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveOutTrRxns, parentRes.model));

  pairAnalysis.parentGainedRxns = parentGainedRxns;
  pairAnalysis.childLostRxns = childLostRxns;

  [pairAnalysis.overlappingTr.child, pairAnalysis.overlappingTrNoInorg.child] = ...
    competitionIndices(childRes.model, childRes.res.flux);
  [pairAnalysis.overlappingTr.parent, pairAnalysis.overlappingTrNoInorg.parent] = ...
    competitionIndices(parentRes.model, parentRes.res.flux);
end

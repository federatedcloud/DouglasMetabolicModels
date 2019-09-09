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

  effluxRxns = findEffluxRxns(parentRes.model);
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
  childActiveExCount = 0;
  parentActiveExCount = 0;
  childActiveTrCount = 0;
  parentActiveTrCount = 0;
  childActiveRxns = {};
  parentActiveRxns = {};
  childActiveExRxns = {};
  parentActiveExRxns = {};
  childActiveTrRxns = {};
  parentActiveTrRxns = {};

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
      if any(strcmp(rxn, effluxRxns))
        childActiveExCount = childActiveExCount + 1;
        childActiveExRxns{end+1} = rxn;
      elseif any(strcmp(rxn, trRxns))
        childActiveTrCount = childActiveTrCount + 1;
        childActiveTrRxns{end+1} = rxn;
        if DEBUG
          fprintf(trRxnFID, '%s\t%d\n', rxn, childRFmap(rxn));
        end
      end
    end
    if abs(parRFmap(rxn)) > fluxActThresh
      parentActiveCount = parentActiveCount + 1;
      parentActiveRxns{end+1} = rxn;
      parIsAct = 1;
      if any(strcmp(rxn, effluxRxns))
        parentActiveExCount = parentActiveExCount + 1;
        parentActiveExRxns{end+1} = rxn;
      elseif any(strcmp(rxn, trRxns))
        parentActiveTrCount = parentActiveTrCount + 1;
        parentActiveTrRxns{end+1} = rxn;
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
  pairAnalysis.cmpEx.child = childActiveExCount;
  pairAnalysis.cmpEx.parent = parentActiveExCount;
  pairAnalysis.cmpTr.child = childActiveTrCount;
  pairAnalysis.cmpTr.parent = parentActiveTrCount;

  pairAnalysis.cmpUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveRxns, childRes.model));
  pairAnalysis.cmpUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveRxns, parentRes.model));
  pairAnalysis.cmpExUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveExRxns, childRes.model));
  pairAnalysis.cmpExUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveExRxns, parentRes.model));
  pairAnalysis.cmpTrUniq.child = ...
    numel(flattenRxnsAcrossSpecies(childActiveTrRxns, childRes.model));
  pairAnalysis.cmpTrUniq.parent = ...
    numel(flattenRxnsAcrossSpecies(parentActiveTrRxns, parentRes.model));

  pairAnalysis.parentGainedRxns = parentGainedRxns;
  pairAnalysis.childLostRxns = childLostRxns;

  [pairAnalysis.compIndices.child, pairAnalysis.overlappingInputs.child] = ...
    competitionIndices(childRes.model, childRes.res.flux);
  [pairAnalysis.compIndices.parent, pairAnalysis.overlappingInputs.parent] = ...
    competitionIndices(parentRes.model, parentRes.res.flux);
end

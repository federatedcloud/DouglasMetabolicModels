%
% childRes  In this sense has a flux derived from a model with
%           one fewer species than the parentRes.
% parentRes See childRes.
%
function pairAnalysis = compareFluxes(childRes, parentRes)
  fluxActThresh = 1e-6
  childRFmap = containers.Map(childRes.model.rxns, childRes.res.flux);
  parentRes % FIXME
  parRFmap = containers.Map(parentRes.model.rxns, parentRes.res.flux);
  rxnsDiff = {};
  childRxnFlux = {};
  parRxnFlux = {};
  parentGainedRxns = {};
  childLostRxns = {};
  pairAnalysis = struct;

  [selExc, selUpt] = findExcRxns(parentRes.model);
  effluxRxns = parentRes.model.rxns(selExc & (~selUpt));
  effluxRxns = filter1d(@(x) startsWith(x,'EX'), effluxRxns);

  % really we should just be able to use the smaller reaction set, as
  % as it should be a subset of the parent reaction set, so no need for this:
  commonRxns = intersect(childRes.model.rxns, parentRes.model.rxns);
  childActiveCount = 0;
  parentActiveCount = 0;
  childActiveExCount = 0;
  parentActiveExCount = 0;
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
      childIsAct = 1;
      if any(contains(effluxRxns, rxn))
        childActiveExCount = childActiveExCount + 1;
      end
    end
    if abs(parRFmap(rxn)) > fluxActThresh
      parentActiveCount = parentActiveCount + 1;
      parIsAct = 1;
      if any(contains(effluxRxns, rxn))
        parentActiveExCount = parentActiveExCount + 1;
      end
    end
    if (parIsAct > childIsAct)
      parentGainedRxns{end+1} = rxn;
    elseif (childIsAct > parIsAct)
      childLostRxns{end+1} = rxn;
    end
  end
  tbl = makeTable(rxnsDiff, childRxnFlux, parRxnFlux);
  childName = strjoin(childRes.model.infoCom.spAbbr, '_');
  parName = strjoin(parentRes.model.infoCom.spAbbr, '_');
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
  pairAnalysis.parentGainedRxns = parentGainedRxns;
  pairAnalysis.childLostRxns = childLostRxns;
end

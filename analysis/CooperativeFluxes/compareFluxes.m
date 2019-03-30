%
% childRes  In this sense has a flux derived from a model with
%           one fewer species than the parentRes.
% parentRes See childRes.
%
function pairAnalysis = compareFluxes(childRes, parentRes)
  childRFmap = containers.Map(childRes.model.rxns, childRes.res.flux);
  parentRes % FIXME
  parRFmap = containers.Map(parentRes.model.rxns, parentRes.res.flux);
  rxnsDiff = {};
  childRxnFlux = {};
  parRxnFlux = {};
  pairAnalysis = struct;

  % really we should just be able to use the smaller reaction set, as
  % as it should be a subset of the parent reaction set, so no need for this:
  commonRxns = intersect(childRes.model.rxns, parentRes.model.rxns);
  for ii = 1:numel(commonRxns)
    rxn = char(commonRxns{ii});
    if sign(childRFmap(rxn)) ~= sign(parRFmap(rxn))
      rxnsDiff{end+1} = rxn;
      childRxnFlux{end+1} = childRFmap(rxn);
      parRxnFlux{end+1} = parRFmap(rxn);
    end
  end
  tbl = makeTable(rxnsDiff, childRxnFlux, parRxnFlux);
  childName = strjoin(childRes.model.infoCom.spAbbr, '_');
  parName = strjoin(parentRes.model.infoCom.spAbbr, '_');
  parNameTbl = parName;
  if strcmp(parName, childName)
    parNameTbl = strjoin({parName, '_'}, '');
  end
  tbl.Properties.VariableNames = {'Reaction', childName, parNameTbl};
  pairAnalysis.label = strjoin({childName, '__vs__', parName}, '');
  pairAnalysis.table = tbl;
end

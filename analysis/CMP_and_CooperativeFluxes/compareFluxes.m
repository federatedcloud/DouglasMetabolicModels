%
% childRes  In this sense has a flux derived from a model with
%           one fewer species than the parentRes.
% parentRes See childRes.
%
function pairAnalysis = compareFluxes(childRes, parentRes, setDEBUG)
  if (nargin < 3)
    setDEBUG = false;
  end

  pairAnalysis = struct;

  pairAnalysis.size.child = numel(childRes.model.infoCom.spAbbr);
  pairAnalysis.size.parent = numel(parentRes.model.infoCom.spAbbr);

  childName = commString(childRes.model);
  parName = commString(parentRes.model);

  pairAnalysis.childName = childName;
  pairAnalysis.parName = parName;
  pairAnalysis.label = strjoin({childName, '__vs__', parName}, '');

  [CMP, tbl] = cmpAnalysis(childRes, parentRes, false, setDEBUG);
  pairAnalysis.table = tbl;
  pairAnalysis.CMP = CMP;
  [CMPnoInorg, ~] = cmpAnalysis(childRes, parentRes, true, setDEBUG)
  pairAnalysis.CMPnoInorg = CMPnoInorg;

  [pairAnalysis.overlappingTr.child, pairAnalysis.overlappingTrNoInorg.child] = ...
    competitionIndices(childRes.model, childRes.res.flux);
  [pairAnalysis.overlappingTr.parent, pairAnalysis.overlappingTrNoInorg.parent] = ...
    competitionIndices(parentRes.model, parentRes.res.flux);
end

% Used to add species prefixes to a reaction; e.g., `IEX_2aeppn` becomes
% `AFIEX_2aeppn`, `APIEX_2aeppn`, etc.
function rxnsFnd = catSpeciesRxnPrefixes(rxns, multiModel, org)
  if nargin > 2
    rxnsFnd = go(rxns, multiModel, org);
  else
    % Do for all species and take the union
    rxnsFnd = cell2mat(cellFlatMap(@(s) go(rxns, multiModel, s), multiModel.infoCom.spAbbr));
  end
  function rxnsFnd = go(rxns, multiModel, org)
    tentativeRxns = cellFlatMap(@(r) strjoin({org, r}, ''), rxns);
    rxnsFnd = filter1d(@(r) r > 0, findRxnIDs(multiModel, tentativeRxns));
  end

end

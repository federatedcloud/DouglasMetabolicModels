% For a given flux vector, from a multi-species model, determines the competition
% index, the interpretation of which is dependent on the number of species
% present in the model.
function activeRxnList = competitionIndex(rxn, rxns, flux);
  fluxThreshold = 1e-6;
  rxnEscaped = regexprep(rxn, '([\[\]])', '\\$1');
  rxnRegex = strjoin({'^[A-Z]{2}I', rxnEscaped, 'tr$'}, '');
  speciesTrRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, rxnRegex)));
  activeRxnList = {};
  if numel(speciesTrRxns) > 0
    speciesTrIxs = cell2mat(cellFlatMap(@(r) find(strcmp(r, rxns)), speciesTrRxns));
    speciesTrFxs = flux(speciesTrIxs);
    activeIxs = find(abs(speciesTrFxs) > fluxThreshold);
    if numel(activeIxs) > 0
      activeFxs = speciesTrFxs(activeIxs);
      activeRxns = speciesTrRxns(activeIxs);
      activeRxnList = cellFlatMap( ...
        @(c) strjoin(c, ':'), ...
        cellzip(activeRxns, strip(cellstr(num2str(activeFxs)))) ...
      );
    end
  end
  activeRxnList
end

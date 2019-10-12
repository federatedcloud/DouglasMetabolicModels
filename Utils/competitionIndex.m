% For a given flux vector, from a multi-species model, determines the competition
% index, the interpretation of which is dependent on the number of species
% present in the model.
function funOut = competitionIndex(rxn, model, flux)
  rxns = model.rxns;
  funOut = {'', {}};

  function metName = lookupMetName()
    rxnIx = find(strcmp(rxn, rxns));
    metNames = model.metNames(find(model.S(:,rxnIx)));
    metName = metNames{1};
  end

  fluxThreshold = 1e-6;
  rxnEscaped = regexprep(rxn, '([\[\]])', '\\$1');
  rxnRegex = strjoin({'^[A-Z]{2}I', rxnEscaped, 'tr$'}, '');
  speciesTrRxns = rxns(cellfun(@(x) ~isempty(x), regexp(rxns, rxnRegex)));
  activeRxnList = {};
  lineOut = '';
  if numel(speciesTrRxns) > 0
    speciesTrIxs = cell2mat(cellFlatMap(@(r) find(strcmp(r, rxns)), speciesTrRxns));
    speciesTrFxs = flux(speciesTrIxs);
    activeIxs = find(abs(speciesTrFxs) > fluxThreshold);
    inIxs = find(speciesTrFxs < -fluxThreshold);
    outIxs = find(speciesTrFxs > fluxThreshold);
    if numel(activeIxs) > 0
      activeFxs = speciesTrFxs(activeIxs);
      activeRxns = speciesTrRxns(activeIxs);
      activeOrgs = cellFlatMap(@(r) r(1:2), activeRxns);
      inRxns = speciesTrRxns(inIxs);
      outRxns = speciesTrRxns(outIxs);
      inOrgs = cellFlatMap(@(r) r(1:2), inRxns);
      outOrgs = cellFlatMap(@(r) r(1:2), outRxns);
      activeRxnList = cellFlatMap(                              ...
        @(c) strjoin(c, ':'),                                   ...
        cellzip(activeOrgs, strip(cellstr(num2str(activeFxs)))) ...
      );
      rxnMetname = lookupMetName();
      netCIx = num2str(sum(sign(-1*activeFxs)));
      inFluxIx = num2str(sum(activeFxs < 0));
      outFluxIx = num2str(sum(activeFxs > 0));
      rxnInfo = strjoin(activeRxnList, '; ');
      lineOut = strjoin({rxnMetname, netCIx, inFluxIx, outFluxIx, rxnInfo}, ', ');
      statsOut = {netCIx, inFluxIx, outFluxIx, inOrgs, outOrgs};
      funOut = {lineOut, statsOut};
    end
  end
end

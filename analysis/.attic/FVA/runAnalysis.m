function [tbl, minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runAnalysis(multiModel, sName)
  fvaMainCols =  {'Reaction', 'SC_Flux', 'Min_Flux', 'Max_Flux'};
  nSpecies = length(multiModel.infoCom.spAbbr);
  growthLabelsMin = cellFlatMap(@(s) strcat(s, '_min_growth'), multiModel.infoCom.spAbbr);
  growthLabelsMax = cellFlatMap(@(s) strcat(s, '_max_growth'), multiModel.infoCom.spAbbr);
  tblColNames = {fvaMainCols{:} growthLabelsMin{:} growthLabelsMax{:}}
  % tblCols = cell(length(fvaMainCols) + 2*nspecies)


  trRxns = findTrRxns(multiModel);
  exRxns = multiModel.rxns(findExcIDs(multiModel));
  fvaRxns = union(trRxns, exRxns);
  fvaRxnIds = findRxnIDs(multiModel, fvaRxns);

  [sol, res] = runSteadyCom(multiModel);
  flux = res.flux(fvaRxnIds);

  opts.rxnNameList = fvaRxns;
  opts.saveFVA = sName;
  [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runSteadyComFVA(multiModel, opts);
  minGRs = num2cell(minFD, 2);
  maxGRs = num2cell(maxFD, 2);

  tblCols = {fvaRxns, flux, minFlux, maxFlux, minGRs{:}, maxGRs{:}}
  tbl = makeTable(tblCols{:});

  tbl.Properties.VariableNames = tblColNames;
  writetable(tbl, strcat(sName, '_table.csv'));
end

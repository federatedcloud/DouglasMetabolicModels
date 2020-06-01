function essentialRxns = checkEssentiality(model, rxns)
  numRxns = numel(rxns);
  essentialRxns = cell(numRxns, 1);
  for ii = 1:numRxns
    modelDel = changeRxnBounds(model, rxns(ii), 0, 'b');
    [sol, res] = runSteadyCom(modelDel);
    fbaRes = optimizeCbModel(modelDel);
    if fbaRes.f < 1e-7 || sum(res.vBM) < 1e-6
      output = struct;
      output.rxn = rxns(ii);
      output.fbaGrowth = fbaRes.f;
      output.scomGrowth = res.GRmax;
      output.vBM = res.vBM;
      essentialRxns{end+1} = output;
    end
  end
  essentialRxns = [essentialRxns{:}];
end

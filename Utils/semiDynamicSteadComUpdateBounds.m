function newLB = semiDynamicSteadComUpdateBounds(model, modelPrior, fluxPrior, essInfo, mediaRxns)
  fluxThresh = 1e-7;
  growThresh = 1e-7;

  newLB = model.lb;

  essRxns = cellFlatMap(@(es) es.rxn{1}, essInfo);
  nonEssRxns = setdiff(model.rxns, essRxns);

  for ri = 1:length(nonEssRxns)
    neRxn = nonEssRxns{ri};
    rxnIxPrior = find(strcmp(modelPrior.rxns, neRxn));
    rxnIxCurr = find(strcmp(model.rxns, neRxn));
    % It is being consumed priorly, so we constrain to zero
    if fluxPrior(rxnIxPrior) < -fluxThresh
      newLB(rxnIxCurr) = 0;
    end    
  end
end

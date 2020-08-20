function newLB = semiDynamicSteadyComUpdateBounds(model, modelPrior, fluxPrior, essInfo)
  fluxThresh = 1e-7;
  growThresh = 1e-7;

  newLB = model.lb;

  essRxns = cellFlatMap(@(es) es.rxn{1}, num2cell(essInfo));
  nonEssRxns = setdiff(model.rxns, essRxns);

  for ri = 1:length(nonEssRxns)
    neRxn = nonEssRxns{ri};
    rxnIxPrior = find(strcmp(modelPrior.rxns, neRxn));
rxnIxCurr = find(strcmp(model.rxns, neRxn));
    % It is being consumed priorly, so we constrain to zero
    if numel(fluxPrior) > 0
      if fluxPrior(rxnIxPrior) < -fluxThresh
        % newLB(rxnIxCurr) = 0;
        newLB(rxnIxCurr) =  0.2 * fluxPrior(rxnIxPrior);
      end
    end    
  end
end

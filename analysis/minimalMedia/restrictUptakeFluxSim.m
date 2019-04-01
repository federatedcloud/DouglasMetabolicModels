function modelRunner = restrictUptakeFluxSim(multiModel, rxnSet)
  function retVal = runRestrictedModel(rxnsKept)
    rxnsToZero = setdiff(rxnsKept, rxnSet);
    nDelRxns = numel(rxnsToZero);
    modelDel = multiModel;
    for ii = 1:nDelRxns
      modelDel = changeRxnBounds(model, rxnsKept{ii}, 0, 'b');
    end
    [sol, result] = runSteadyCom(modelDel);
    retVal.model = multiModel;
    retVal.sol = sol;
    retVal.res = result;
    retVal.rxnsKept = rxnsKept;
  end
  modelRunner = @runRestrictedModel;
end

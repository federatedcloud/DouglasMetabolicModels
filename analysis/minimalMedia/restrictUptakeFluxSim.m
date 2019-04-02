function modelRunner = restrictUptakeFluxSim(multiModel, rxnSet)
  function retVal = runRestrictedModel(rxnsKept)
    rxnsToZero = setdiff(rxnSet, rxnsKept);
    nDelRxns = numel(rxnsToZero);
    modelDel = multiModel;
    for ii = 1:nDelRxns
      modelDel = changeRxnBounds(multiModel, rxnsKept(ii), 0, 'b');
    end
    [sol, result] = runSteadyCom(modelDel);
    retVal.model = modelDel;
    retVal.sol = sol;
    retVal.res = result;
    retVal.rxnsKept = rxnsKept;
  end
  modelRunner = @runRestrictedModel;
end

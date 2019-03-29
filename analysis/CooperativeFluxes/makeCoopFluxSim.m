function modelRunner = makeCoopFluxSim(modelMap)
  function retVal = makeAndRunModel(modelKeys)
    retVal = struct;
    multiModel = makeMultiModel(modelKeys, modelMap, 'rich');
    [sol, result] = runSteadyCom(multiModel);
    retVal.model = multiModel;
    retVal.sol = sol;
    retVal.res = result;
  end
  modelRunner = @makeAndRunModel;
end

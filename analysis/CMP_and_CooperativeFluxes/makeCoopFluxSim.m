function modelRunner = makeCoopFluxSim(modelMap, mediaType)
  function retVal = makeAndRunModel(modelKeys)
    retVal = struct;
    multiModel = makeMultiModel(modelKeys, modelMap, mediaType);
    [sol, result] = runSteadyCom(multiModel);
    retVal.model = multiModel;
    retVal.sol = sol;
    retVal.res = result;
  end
  modelRunner = @makeAndRunModel;
end

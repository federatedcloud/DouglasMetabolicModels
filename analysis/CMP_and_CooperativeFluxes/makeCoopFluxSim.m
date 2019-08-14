function modelRunner = makeCoopFluxSim(modelMap, mediaType)
  function retVal = makeAndRunModel(modelKeys)
    retVal = struct;
    multiModel = makeMultiModel(modelKeys, modelMap, mediaType);
    runName = strcat(mediaType, '__', commString(multiModel));
    [sol, result] = runSteadyComAvg(multiModel, simName(runName));
    retVal.model = multiModel;
    retVal.sol = sol;
    retVal.res = result;
  end
  modelRunner = @makeAndRunModel;
end

function modelRunner = makeCoopFluxSim(modelMap, mediaType)
  function retVal = makeAndRunModel(modelKeys)
    retVal = struct;
    multiModel = makeMultiModel(modelKeys, modelMap, mediaType);
    runName = strcat(mediaType, '__', commString(multiModel));
    dOpts = steadyComDefs(multiModel, false);
    namedOpts = simName(runName, dOpts);
    [sol, result] = runSteadyComFVAMedoid(multiModel, namedOpts);
    retVal.model = multiModel;
    retVal.sol = sol;
    retVal.res = result;
  end
  modelRunner = @makeAndRunModel;
end

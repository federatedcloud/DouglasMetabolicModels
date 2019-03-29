%
% Just a simple example to demonstrate how to run an analysis without
% having to pass in models each time; instead, the names of submodels are passed in.
%
% This is useful for running closure functions like this one as part of some higher
% -level analysis, using e.g. cellPowerSetFilter to callt he closure created by this
% function.
%
function modelRunner = closureExample(modelMap)
  function [sol, result] = makeAndRunModel(modelKeys)
    multiModel = makeMultiModel(modelKeys, modelMap, 'rich');
    [sol, result] = runSteadyCom(multiModel);
  end
  modelRunner = @makeAndRunModel;
end

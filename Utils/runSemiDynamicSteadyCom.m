function schedRes = runSemiDynamicSteadyCom(modelMap, schedule, optsOverride)

  optsOverride = struct;
  % optsOverride.minNorm = 1; % doesn't appear to work with solvaCobraLP? currently?
  schedRes = semiDynamicSteadyCom(modelMap, schedule, {}, optsOverride);

  % TODO: Will worry about fixSCRes later for semiDynamicSteadyCom
  % result = fixSCRes(result);
end

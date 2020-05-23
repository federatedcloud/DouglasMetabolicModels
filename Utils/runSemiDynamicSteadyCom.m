function schedRes = runSemiDynamicSteadyCom(modelMap, schedule, optsOverride)

  optsOverride = struct;
  % optsOverride.minNorm = 1; % doesn't appear to work with solvaCobraLP? currently?
  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  schedRes = semiDynamicSteadyCom(modelMap, schedule, {}, optsOverride);

  % TODO: Will worry about fixSCRes later for semiDynamicSteadyCom
  % result = fixSCRes(result);

  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

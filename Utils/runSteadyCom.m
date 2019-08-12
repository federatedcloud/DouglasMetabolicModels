function [sol, result] = runSteadyCom(multiModel)

  options = steadyComDefs(multiModel);

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  [sol result] = SteadyCom(multiModel, options);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

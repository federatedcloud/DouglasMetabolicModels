function [sol, result, LP, LPminNorm] = runSteadyCom(multiModel, optsIn)

  if nargin < 2
    options  = steadyComDefs(multiModel);
  else
    options = optsIn;
  end

  % options.minNorm = 1;
  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-5);
  [sol result LP LPminNorm] = SteadyCom(multiModel, options);
  result = fixSCRes(result);

  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

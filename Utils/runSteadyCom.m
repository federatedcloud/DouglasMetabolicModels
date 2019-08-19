function [sol, result, LP, LPminNorm] = runSteadyCom(multiModel, options)

  defOpts = steadyComDefs(multiModel);
  if nargin < 2
    options  = defOpts;
  end

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  [sol result LP LPminNorm] = SteadyCom(multiModel, options);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

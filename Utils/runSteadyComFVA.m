function [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runSteadyComFVA(multiModel, simName, otherOpts);

  defOpts = steadyComDefs(multiModel);
  options = defOpts;
  if nargin > 2
    options = mergeStructs(defOpts, otherOpts); % TODO: semicolon
  end

  options.saveFVA = simName;
  options.saveFre = 0.05;
  options.threads = 0;

  % TODO: use all model rxns?

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = SteadyComFVA(multiModel, options);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

function flux = runGeometricSC(multiModel)

  options = steadyComDefs(multiModel);

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  flux = geometricSC(multiModel, options, 'flexRel', 0, 'epsilon', 1e-6);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);
end

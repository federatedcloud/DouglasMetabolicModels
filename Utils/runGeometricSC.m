function flux = runGeometricSC(multiModel)

  options = steadyComDefs(multiModel);

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  flux = geometricSC(multiModel, options, 'flexRel', 1e-3, 'epsilon', 1e-6);
end

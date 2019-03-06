function [sol, result] = runSteadyCom(multiModel)
% Convenience wrapper for running SteadyCom for our models
% INPUT: multiModel
% OUTPUT: [sol, result]

  options=struct();
  options.algorithm = 3;

  % We're assuming proportial in this case (make a flag later)
  %
  % nSpecies = length(multiModel.infoCom.spAbbr);
  % options.BMrhs = [nSpecies];
  % options.BMcsense = 'E';
  % bmCon(1:nSpecies) = 1;
  % options.BMcon = bmCon;

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  [sol result] = SteadyCom(multiModel, options);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);

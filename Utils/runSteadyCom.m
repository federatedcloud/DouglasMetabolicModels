function [sol, result] = runSteadyCom(multiModel)
% Convenience wrapper for running SteadyCom for our models

  options=struct();
  options.algorithm = 3;

  % We're assuming proportial in this case (make a flag later)
  % options.BMrhs = [0];
  % options.BMcsense = 'E';
  % options.BMcon = [2 -1]  % VARY biomass constraint here
  [sol result] = SteadyCom(multiModel, options);

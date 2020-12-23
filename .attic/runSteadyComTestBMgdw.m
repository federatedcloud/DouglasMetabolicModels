function [sol, result] = runSteadyComTestBMgdw(multiModel)
% Convenience wrapper for running SteadyCom for our models
% INPUT: multiModel
% OUTPUT: [sol, result]

  options=struct();
  options.algorithm = 3;

  % We're assuming proportial in this case (make a flag later)
  %
  % nSpecies = length(multiModel.infoCom.spAbbr);
  % options.BMrhs = [nSpecies];
  % options.BMcsense = 'G';
  % bmCon(1:nSpecies) = 1;
  % options.BMcon = bmCon;


  acetoIndexes = find(startsWith(multiModel.infoCom.spAbbr, 'A'))
  % options.BMobj = zeros(1, length(multiModel.infoCom.spAbbr));
  % options.BMobj(acetoIndexes) = 1;
  options.BMobj = [1 1 1 1 1];
  % options.BMobj
  % size(options.BMobj)

  options.BMcon=[1 1 1 1 1
                 1 0 0 0 0;
                 0 1 0 0 0;
                 0 0 1 0 0;
                 0 0 0 1 0;
                 0 0 0 0 1
  ];

  options.BMrhs=[50 0.01 0.01 0.01 0.01 0.01];
  options.BMcsense=['EGGGGG'];

  origFeasTol = getCobraSolverParams('LP', 'feasTol');
  changeCobraSolverParams('LP', 'feasTol', 1e-8);
  [sol result] = SteadyCom(multiModel, options);
  changeCobraSolverParams('LP', 'feasTol', origFeasTol);


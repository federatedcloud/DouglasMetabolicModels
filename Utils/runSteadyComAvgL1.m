% Just like runSteadyCom, but uses FVA to compute the mean flux and growth rates.
function [sol, res, LP, LPminNorm] = runSteadyComAvgL1(multiModel, otherOpts);
  tic;

  nSpecies = length(multiModel.infoCom.spAbbr);

  if nargin > 1
    fvaOpts = otherOpts;
    if ~isfield('rxnNameList', fvaOpts)
      fvaOpts.rxnNameList = multiModel.rxns;
    end
  else
    fvaOpts = struct();
    fvaOpts.rxnNameList = multiModel.rxns;
  end

  [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runSteadyComFVA(multiModel, fvaOpts);

  % This may cause the model to be infeasible:
  % multiModel.lb = minFlux;
  % muliModel.ub = maxFlux;

  bmrhs = mean([minFD maxFD]')/GRvector

  L1Opts = steadyComDefs(multiModel);
  L1Opts.BMrhs = bmrhs;
  L1Opts.BMcsense = [strjoin(repmat({'E'}, nSpecies, 1), '')];
  L1Opts.minNorm = 1;
  [sol, res, LP, LPminNorm] = runSteadyCom(multiModel, L1Opts);

  sol.time = toc;

end

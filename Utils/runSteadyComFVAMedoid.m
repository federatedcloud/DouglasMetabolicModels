% Just like runSteadyCom, but uses FVA to compute the mean flux and growth rates.
function [sol, resMed, LP, LPminNorm] = runSteadyComFVAMedoid(multiModel, otherOpts);
  tic;

  nSpecies = length(multiModel.infoCom.spAbbr);

  epsilon = 1e-6;

  if nargin > 1
    fvaOpts = otherOpts;
  else
    fvaOpts = struct();
  end

  fvaOpts.rxnNameList = multiModel.rxns;
  % ^ we record all fluxes for determining medoid flux
  fvaOpts.minNorm = 1;

  [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runSteadyComFVA(multiModel, fvaOpts);
  vBMmedoid = medoid([minFD maxFD])
  % vBMmedoid = [0.0001, 0.0001, 0.0003, 0.0149, 0.8249]'; % For testing
  mu = sum(vBMmedoid);
  if mu > 0
    vbm = vBMmedoid/mu - epsilon;
    grMaxMean = mean(GRvector)
    bmRHS = vbm / grMaxMean;
    fvaOpts.BMcon = diag(ones(nSpecies, 1));
    fvaOpts.BMrhs = bmRHS;
    fvaOpts.BMcsense = [strjoin(repmat({'G'}, nSpecies, 1), '')];
  end
  [sol, resMed, LP, LPminNorm] = runSteadyCom(multiModel, fvaOpts);

  sol.time = toc;

end

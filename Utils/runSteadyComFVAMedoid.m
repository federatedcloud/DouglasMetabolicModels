% Just like runSteadyCom, but uses FVA to compute the mean flux and growth rates.
function [sol, resMed, LP, LPminNorm] = runSteadyComFVAMedoid(multiModel, otherOpts);
  tic;

  nSpecies = length(multiModel.infoCom.spAbbr);

  if nargin > 1
    fvaOpts = otherOpts;
  else
    fvaOpts = struct();
  end

  fvaOpts.rxnNameList = multiModel.rxns;
  fvaOpts.rxnFluxList = multiModel.rxns;
  % ^ we record all fluxes for determining medoid flux
  fvaOpts.minNorm = 1;

  [minFlux, maxFlux, minFD, maxFD, GRvector, result, LP] = runSteadyComFVA(multiModel, fvaOpts);


  sol = struct();
  sol.LP = LP;

  resMed = struct();
  resMed.GRmax = GRvector;
  resMed.flux = medoid([minFD maxFD]);
  resMed.vBM = resMed.flux(find(multiModel.c));
  resMed.BM = resMed.vBM / GRvector;

  resMed.minFlux = minFlux;
  resMed.maxFlux = maxFlux;
  resMed.minFD = minFD;
  resMed.maxFD = maxFD;
  resMed.resultSC = result;

  sol.time = toc;

end

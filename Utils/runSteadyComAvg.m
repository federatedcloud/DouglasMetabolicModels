% Just like runSteadyCom, but uses FVA to compute the mean flux and growth rates.
function [sol, resAvg] = runSteadyComAvg(multiModel, otherOpts);
  tic;

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

  sol = struct();
  sol.LP = LP;

  resAvg = struct();
  resAvg.GRmax = GRvector;
  resAvg.flux = (minFlux + maxFlux)/2;
  resAvg.vBM = mean(((minFD + maxFD)/2)')';
  resAvg.BM = resAvg.vBM / GRvector;

  resAvg.minFlux = minFlux;
  resAvg.maxFlux = maxFlux;
  resAvg.minFD = minFD;
  resAvg.maxFD = maxFD;
  resAvg.resultSC = result;

  sol.time = toc;
end

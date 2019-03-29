%
% This is a recursive function, so it will not work on large sets.
%
% fun is the operation applied to each subset, parent value pair.
%     It is NOT applied to the empty set
%
% pred is applied to the output of fun;
% if true, then the result is eventually returned, and the function
% recurses.
%
% comp: it is up to the function implementation to record which sets
%       are being compared in the output
%
% Example: To just show subsets with parents in comps:
% cellPowerSetFilter(@(x) x, @(x) true, someSet, @(v,pv) {pv,v})
%
function [ssVals, comps, memFun] = cellPowerSetFilter(fun, pred, carray, compare)
  setInit = {carray{:}};
  memFun = memoize(fun);
  memFun.CacheSize = 2.^(numel(setInit));
  ssVals = {};
  comps = {};
  valInit = memFun(setInit);
  loop(setInit, valInit);

  function loop(ssetIn, parentValue)
    ssVals{end+1} = memFun(ssetIn);
    comps{end+1} = compare(ssVals{end}, parentValue);
    nSubs = numel(ssetIn);
    if pred(ssVals{end}) && nSubs > 1
      for ii = 1:nSubs
        sset = ssetIn;
        sset(ii) = [];
        loop(sset, ssetIn);
      end
    end
  end

end

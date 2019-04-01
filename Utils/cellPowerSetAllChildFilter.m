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
% compare: it is up to the function implementation to record which sets
%          are being compared in the output
%
% Example: To just show subsets with parents in comps:
% cellPowerSetFilter(@(x) x, @(x) true, someSet, @(v,pv) {pv,v})
%
function [ssVals, comps, memFun] = cellPowerSetAllChildFilter(fun, pred, carray, compare, childAnyAll)
  setInit = {carray{:}};
  memFun = memoize(fun);
  memFun.CacheSize = 2.^(numel(setInit));
  ssVals = {};
  comps = {};
  valInit = memFun(setInit);
  loop(setInit, valInit);

  function bval = anyOrAll(pType, predVals)
    if strcmp(pType, 'all')
      bval = all(cell2mat(predVals));
    elseif strcmp(pType, 'any')
      bval = any(cell2mat(predVals));
    else
      error('pType must be either "any" or "all".')
    end
  end

  function loop(ssetIn, parentValue)
    currentValues = {};
    nSubs = numel(ssetIn);
    for ii = 1:nSubs
      sset = ssetIn;
      sset(ii) = [];
      currentValues{ii} = memFun(sset);
    end
    predVals = cellfun(pred, currentValues, 'UniformOutput',false);
    if anyOrAll(childAnyAll, predVals) && nSubs > 1
      for ii = 1:nSubs
        sset = ssetIn;
        sset(ii) = [];
        loop(sset, memFun(sset));
      end
    end
    ssVals = {ssVals{:}  currentValues{:}};
    comps{end+1} = compare(currentValues, parentValue);
  end

end

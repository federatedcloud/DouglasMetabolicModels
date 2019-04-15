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
  memFun = memoize(fun); % Too much memory?
  memFun.CacheSize = 2.^(numel(setInit));
  memFun = fun;
  ssVals = {};
  comps = {};
  valInit = memFun(setInit);

  ppool = gcp;
  poolsize = 0;
  if ~isempty(ppool)
    poolsize = ppool.NumWorkers;
  end
  disp(strjoin({'pool size is :', num2str(poolsize)}));

  environment = getEnvironment();
  loop(setInit, valInit, environment);

  function bval = anyOrAll(pType, predVals)
    if strcmp(pType, 'all')
      bval = all(cell2mat(predVals));
    elseif strcmp(pType, 'any')
      bval = any(cell2mat(predVals));
    else
      error('pType must be either "any" or "all".')
    end
  end

  function loop(ssetIn, parentValue, cbenv)
    nSubs = numel(ssetIn);
    currentValues = cell(1, nSubs);
    if nSubs > 0
      parfor ii = 1:nSubs
        restoreEnvironment(cbenv);
        disp(strjoin({'Running simulation of subset size:', num2str(nSubs-1)}));
        sset = ssetIn;
        sset(ii) = [];
        disp(ssetIn); %DEBUG
        disp(sset); %DEBUG
        currentValues{ii} = memFun(sset);
      end
      predVals = cellfun(pred, currentValues, 'UniformOutput', false);
      for ii = find(cell2mat(predVals))
        sset = ssetIn;
        sset(ii) = [];
        loop(sset, memFun(sset), cbenv);
      end
      %ssVals = {ssVals{:}  currentValues{:}};
      % We aren't interested in storing individual model values;
      % besides, it takes too much memory.
      comps{end+1} = compare(currentValues, parentValue);
    end

    end
end

function modelOut = capBounds(model, bound)
  bound = abs(bound);
  modelOut = model;
  nRxns = numel(model.lb);
  for ii = 1:nRxns
    lbSign = sign(modelOut.lb(ii));
    if lbSign > 0
      modelOut.lb(ii) = min(modelOut.lb(ii), bound);
    else
      modelOut.lb(ii) = max(modelOut.lb(ii), -bound);
    end
    ubSign = sign(modelOut.ub(ii));
    if ubSign > 0
      modelOut.ub(ii) = min(modelOut.ub(ii), bound);
    else
      modelOut.ub(ii) = max(modelOut.ub(ii), -bound);
    end
  end
end

function restoreMisses = findRestoreExcMisses(model)
% Do a quick sanity check
  restoreMisses = intersect(find(model.lb < -100), findExcIDs(model));
end

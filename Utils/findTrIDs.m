function trIDs = findTrIDs(model)
  trRxns = findTrRxns(model);
  trIDs = cell2mat(cellFlatMap(@(r) find(strcmp(r, model.rxns)), trRxns))';
end

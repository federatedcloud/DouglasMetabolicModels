function trRxns = findTrRxns(multiModel)
  trRxns = multiModel.rxns(endsWith(multiModel.rxns, '[u]tr'));
end

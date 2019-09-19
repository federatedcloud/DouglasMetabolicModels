function bmRxns = findBiomassRxnIds(model)
  bmRxns = find(contains(model.rxns, 'biomass'));
end

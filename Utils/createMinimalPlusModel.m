function modelOut = createMinimalPlusModel(multiModel)

% taken from models/5.models.020419/media.xlsx

rxns = {
  'EX_glu-L[u]',
  'EX_ile-L[u]',
  'EX_leu-L[u]',
  'EX_met-L[u]',
  'EX_val-L[u]',
  'EX_nh4[u]',
  'EX_glc-D[u]',
  'EX_glyc[u]',
  'EX_h[u]',
  'EX_h2o[u]',
  'EX_o2[u]',
  'EX_pi[u]',
  'EX_so4[u]',
  'EX_h2s[u]',
  'EX_btn[u]',
  'EX_nac[u]',
  'EX_pnto-R[u]',
  'EX_pydam[u]',
  'EX_pydxn[u]',
  'EX_pydx5p[u]'
};

lower_bounds = {
  -5,
  -5,
  -5,
  -5,
  -5,
  -5,
  -10,
  -5,
  -1,
  -10,
  -20,
  -1,
  -5,
  -5,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1
};

modelOut = multiModel;
rxnLbMap = containers.Map(rxns, lower_bounds);

excIDs = findExcIDs(modelOut);
modelOut.lb(excIDs) = 0;
for rxn = keys(rxnLbMap)
  rxn = char(rxn);
  rxnIdx = find(strcmp(rxn, modelOut.rxns));
  modelOut.lb(rxnIdx) = rxnLbMap(rxn);
end

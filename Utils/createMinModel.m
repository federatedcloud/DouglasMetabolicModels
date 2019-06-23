function [modelOut, rxns, lbs] = createMinModel(multiModel, removeRxns)

rxns = {
  'EX_nh4[u]',
  'EX_so4[u]',
  'EX_glc-D[u]',
  'EX_ca2[u]',
  'EX_cl[u]',
  'EX_cobalt2[u]',
  'EX_cu2[u]',
  'EX_fe2[u]',
  'EX_fe3[u]',
  'EX_h[u]',
  'EX_h2o[u]',
  'EX_k[u]',
  'EX_mg2[u]',
  'EX_mn2[u]',
  'EX_mobd[u]',
  'EX_na1[u]',
  'EX_ni2[u]',
  'EX_o2[u]',
  'EX_pi[u]',
  'EX_zn2[u]',
  % Fist tier didnt work, trying more reactions:

  'EX_glyc[u]',
  'EX_pydx5p[u]'
};

lbs = {
  -5,
  -5,
  -10,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -10,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1,
  -20,
  -1,
  -1,

   % Second tier (blue)
  -5,
  -1
};

if exist('removeRxns', 'var')
  modelOut = updateBounds(multiModel, rxns, lbs, removeRxns);
else
  modelOut = updateBounds(multiModel, rxns, lbs);
end


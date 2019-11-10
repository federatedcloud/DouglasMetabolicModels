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
	-0.05,
	-0.05,
	-0.1,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.1,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-0.01,
	-2,
	-0.01,
	-0.01,


   % Second tier (blue)
	-0.05,
	-0.01

};

if exist('removeRxns', 'var')
  modelOut = updateBounds(multiModel, rxns, lbs, removeRxns);
else
  modelOut = updateBounds(multiModel, rxns, lbs);
end



function [modelOut, rxns, lbs] = createBiologModel(multiModel, removeRxns)

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

  % Added to minimal media for Biolog
  'EX_arg-L[u]',
  'EX_asn-L[u]',
  'EX_ile-L[u]',
  'EX_leu-L[u]',
  'EX_orn[u]',
  'EX_trp-L[u]',
  'EX_val-L[u]',

  'EX_actn-R[u]',
  'EX_akg[u]',
  'EX_succoa[u]',

  'EX_h2s[u]',
  'EX_so4[u]',
  'EX_btn[u]',
  'EX_nmn[u]',
  'EX_pdx5p[u]'
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
	-0.01,

   % Biolog
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05,
	-0.05
};

if exist('removeRxns', 'var')
  modelOut = updateBounds(multiModel, rxns, lbs, removeRxns);
else
  modelOut = updateBounds(multiModel, rxns, lbs);
end



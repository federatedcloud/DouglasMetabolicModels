function [modelOut, rxns, lbs] = createMinimalPlusModel(multiModel, removeRxns)

% taken from models/5.models.020419/media.xlsx

rxns = {
  'EX_26dap-M[u]'
, 'EX_ala-L[u]'
, 'EX_arg-L[u]'
, 'EX_asn-L[u]'
, 'EX_asp-L[u]'
, 'EX_cys-L[u]'
, 'EX_gln-L[u]'
, 'EX_glu-L[u]'
, 'EX_gly[u]'
, 'EX_his-L[u]'
, 'EX_ile-L[u]'
, 'EX_leu-L[u]'
, 'EX_lys-L[u]'
, 'EX_met-L[u]'
, 'EX_nh4[u]'
, 'EX_phe-L[u]'
, 'EX_pro-L[u]'
, 'EX_ptrc[u]'
, 'EX_ser-D[u]'
, 'EX_ser-L[u]'
, 'EX_spmd[u]'
, 'EX_thr-L[u]'
, 'EX_trp-L[u]'
, 'EX_tyr-L[u]'
, 'EX_val-L[u]'
, 'EX_glc-D[u]'
, 'EX_glyc[u]'
, 'EX_ca2[u]'
, 'EX_cl[u]'
, 'EX_cobalt2[u]'
, 'EX_cu2[u]'
, 'EX_fe2[u]'
, 'EX_fe3[u]'
, 'EX_h[u]'
, 'EX_h2o[u]'
, 'EX_h2s[u]'
, 'EX_k[u]'
, 'EX_mg2[u]'
, 'EX_mn2[u]'
, 'EX_mobd[u]'
, 'EX_na1[u]'
, 'EX_ni2[u]'
, 'EX_o2[u]'
, 'EX_pi[u]'
, 'EX_so4[u]'
, 'EX_zn2[u]'
, 'EX_4ahmmp[u]'
, 'EX_5mthf[u]'
, 'EX_btn[u]'
, 'EX_dhpt[u]'
, 'EX_dxyl5p[u]'
, 'EX_nac[u]'
, 'EX_pnto-R[u]'
, 'EX_pydam[u]'
, 'EX_pydx5p[u]'
, 'EX_pydxn[u]'

};

lbs = {
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
-0.1,
-0.05,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.1,
-0.05,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-2,
-0.01,
-0.05,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01,
-0.01
};

if exist('removeRxns', 'var')
  modelOut = updateBounds(multiModel, rxns, lbs, removeRxns);
else
  modelOut = updateBounds(multiModel, rxns, lbs);
end

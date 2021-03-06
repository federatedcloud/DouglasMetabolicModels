function [modelOut, rxns, lbs] = createRichModel(multiModel, removeRxns)

aminoMult = 1;
peptMult = 1;
carbonMult = 1;
otherMult = 1;

rxnLbCell = {
  'EX_26dap-M[u]', -0.5 * aminoMult
  'EX_ala-D[u]', -0.05 * aminoMult
  'EX_ala-L[u]', -0.5 * aminoMult
  'EX_arg-L[u]', -0.5 * aminoMult
  'EX_asn-L[u]', -0.5 * aminoMult
  'EX_asp-L[u]', -0.5 * aminoMult
  'EX_chol[u]', -0.05 * aminoMult
  'EX_chor[u]', -0.05 * aminoMult
  'EX_cys-L[u]'	-0.5 * aminoMult
  'EX_gln-L[u]'	-0.5 * aminoMult
  'EX_glu-L[u]'	-0.5 * aminoMult
  'EX_gly[u]', -0.5 * aminoMult
  'EX_glyb[u]'	-0.05 * aminoMult
  'EX_hcys-L[u]'	-0.05 * aminoMult
  'EX_his-L[u]', -0.5 * aminoMult
  'EX_hista[u]', -0.05 * aminoMult
  'EX_ile-L[u]', -0.5 * aminoMult
  'EX_leu-L[u]', -0.5 * aminoMult
  'EX_lys-L[u]', -0.5 * aminoMult
  'EX_met-D[u]', -0.05 * aminoMult
  'EX_met-L[u]', -0.5 * aminoMult
  'EX_metsox-S-L[u]', -0.05 * aminoMult
  'EX_orn[u]', -0.05 * aminoMult
  'EX_phe-L[u]', -0.5 * aminoMult
  'EX_pro-L[u]', -0.5 * aminoMult
  'EX_ser-D[u]', -0.5 * aminoMult
  'EX_ser-L[u]', -0.5 * aminoMult
  'EX_thr-L[u]', -0.5 * aminoMult
  'EX_trp-L[u]', -0.5 * aminoMult
  'EX_tyr-L[u]', -0.5 * aminoMult
  'EX_val-L[u]', -0.5 * aminoMult
  'EX_12ppd-R[u]', -0.05 * carbonMult
  'EX_2aeppn[u]', -0.05 * carbonMult
  'EX_2ddglcn[u]', -0.05 * carbonMult
  'EX_2hxic-L[u]', -0.05 * carbonMult
  'EX_2mba[u]', -0.05 * carbonMult
  'EX_2mbal[u]', -0.05 * carbonMult
  'EX_2mbol[u]', -0.05 * carbonMult
  'EX_2mpa[u]', -0.05 * carbonMult
  'EX_2mpal[u]', -0.05 * carbonMult
  'EX_2mpol[u]', -0.05 * carbonMult
  'EX_34hplac[u]', -0.05 * carbonMult
  'EX_3mba[u]', -0.05 * carbonMult
  'EX_3mbal[u]', -0.05 * carbonMult
  'EX_3mbol[u]', -0.05 * carbonMult
  'EX_4abut[u]', -0.05 * carbonMult
  'EX_4abz[u]', -0.05 * carbonMult
  'EX_aacoa[u]', -0.05 * carbonMult
  'EX_ac[u]', -0.05 * carbonMult
  'EX_acald[u]', -0.05 * carbonMult
  'EX_acetol[u]', -0.05 * carbonMult
  'EX_acgala[u]', -0.05 * carbonMult
  'EX_acgam[u]', -0.05 * carbonMult
  'EX_actn-R[u]', -0.05 * carbonMult
  'EX_actn-S[u]', -0.05 * carbonMult
  'EX_akg[u]', -0.05 * carbonMult
  'EX_arab-L[u]', -0.05 * carbonMult
  'EX_btd-RR[u]', -0.05 * carbonMult
  'EX_btd-SS[u]', -0.05 * carbonMult
  'EX_bzal[u]', -0.05 * carbonMult
  'EX_cellb[u]', -0.05 * carbonMult
  'EX_cit[u]', -0.05 * carbonMult
  'EX_dha[u]', -0.05 * carbonMult
  'EX_diact[u]', -0.05 * carbonMult
  'EX_etha[u]', -0.05 * carbonMult
  'EX_etoh[u]', -0.05 * carbonMult
  'EX_fald[u]', -0.05 * carbonMult
  'EX_for[u]', -0.05 * carbonMult
  'EX_fru[u]', -0.05 * carbonMult
  'EX_fuc-L[u]', -0.05 * carbonMult
  'EX_fum[u]', -0.05 * carbonMult
  'EX_gal[u]', -0.05 * carbonMult
  'EX_galt[u]', -0.05 * carbonMult
  'EX_glc-D[u]', -1 * carbonMult
  'EX_glcn[u]', -0.05 * carbonMult
  'EX_glyc[u]', -0.5 * carbonMult
  'EX_glyc3p[u]', -0.05 * carbonMult
  'EX_glyclt[u]', -0.05 * carbonMult
  'EX_imlac[u]', -0.05 * carbonMult
  'EX_indlac[u]', -0.05 * carbonMult
  'EX_lac-D[u]', -0.05 * carbonMult
  'EX_lac-L[u]', -0.05 * carbonMult
  'EX_lcts[u]', -0.05 * carbonMult
  'EX_mal-D[u]', -0.05 * carbonMult
  'EX_mal-L[u]', -0.05 * carbonMult
  'EX_malt[u]', -0.05 * carbonMult
  'EX_malthx[u]', -0.05 * carbonMult
  'EX_maltpt[u]', -0.05 * carbonMult
  'EX_malttr[u]', -0.05 * carbonMult
  'EX_maltttr[u]', -0.05 * carbonMult
  'EX_man[u]', -0.05 * carbonMult
  'EX_melib[u]', -0.05 * carbonMult
  'EX_methal[u]', -0.05 * carbonMult
  'EX_mnl[u]', -0.05 * carbonMult
  'EX_mthgxl[u]', -0.05 * carbonMult
  'EX_orot[u]', -0.05 * carbonMult
  'EX_pacald[u]', -0.05 * carbonMult
  'EX_pea[u]', -0.05 * carbonMult
  'EX_phenol[u]', -0.05 * carbonMult
  'EX_phlac[u]', -0.05 * carbonMult
  'EX_pyr[u]', -0.05 * carbonMult
  'EX_rib-D[u]', -0.05 * carbonMult
  'EX_sbt-D[u]', -0.05 * carbonMult
  'EX_succ[u]', -0.05 * carbonMult
  'EX_succoa[u]', -0.05 * carbonMult
  'EX_sucr[u]', -0.05 * carbonMult
  'EX_tre[u]', -0.05 * carbonMult
  'EX_xyl-D[u]', -0.05 * carbonMult
  'EX_aso3[u]', -0.05 * otherMult
  'EX_ca2[u]', -0.1 * otherMult
  'EX_cbi[u]', -0.05 * otherMult
  'EX_cbl1[u]', -0.05 * otherMult
  'EX_cd2[u]', -0.05 * otherMult
  'EX_cl[u]', -0.1 * otherMult
  'EX_co2[u]', -0.05 * otherMult
  'EX_cobalt2[u]', -0.1 * otherMult
  'EX_cu[u]', -0.05 * otherMult
  'EX_cu2[u]', -0.1 * otherMult
  'EX_fe2[u]', -0.1 * otherMult
  'EX_fe3[u]', -0.1 * otherMult
  'EX_h[u]', -0.1 * otherMult
  'EX_h2o[u]', -1 * otherMult
  'EX_hg2[u]', -0.05 * otherMult
  'EX_k[u]', -0.1 * otherMult
  'EX_mg2[u]', -0.1 * otherMult
  'EX_mn2[u]', -0.1 * otherMult
  'EX_mobd[u]', -0.1 * otherMult
  'EX_na1[u]', -0.1 * otherMult
  'EX_ni2[u]', -0.1 * otherMult
  'EX_o2[u]', -2 * otherMult
  'EX_Pb[u]', -0.05 * otherMult
  'EX_pi[u]', -0.1 * otherMult
  'EX_zn2[u]', -0.1 * otherMult
  'EX_nh4[u]', -0.5 * otherMult
  'EX_no2[u]', -0.05 * otherMult
  'EX_no3[u]', -0.05 * otherMult
  'EX_urea[u]', -0.05 * otherMult
  'EX_5mtr[u]', -0.05 * otherMult
  'EX_ade[u]', -0.05 * otherMult
  'EX_adn[u]', -0.05 * otherMult
  'EX_alltn[u]', -0.05 * otherMult
  'EX_csn[u]', -0.05 * otherMult
  'EX_cytd[u]', -0.05 * otherMult
  'EX_dad-2[u]', -0.05 * otherMult
  'EX_dcyt[u]', -0.05 * otherMult
  'EX_drib[u]', -0.05 * otherMult
  'EX_duri[u]', -0.05 * otherMult
  'EX_gua[u]', -0.05 * otherMult
  'EX_hxan[u]', -0.05 * otherMult
  'EX_ins[u]', -0.05 * otherMult
  'EX_thymd[u]', -0.05 * otherMult
  'EX_ura[u]', -0.05 * otherMult
  'EX_uri[u]', -0.05 * otherMult
  'EX_xan[u]', -0.05 * otherMult
  'EX_alaasp[u]', -0.05 * peptMult
  'EX_alagln[u]', -0.05 * peptMult
  'EX_alaglu[u]', -0.05 * peptMult
  'EX_alagly[u]', -0.05 * peptMult
  'EX_alahis[u]', -0.05 * peptMult
  'EX_alaleu[u]', -0.05 * peptMult
  'EX_alathr[u]', -0.05 * peptMult
  'EX_cgly[u]', -0.05 * peptMult
  'EX_glyasn[u]', -0.05 * peptMult
  'EX_glyasp[u]', -0.05 * peptMult
  'EX_glycys[u]', -0.05 * peptMult
  'EX_glygln[u]', -0.05 * peptMult
  'EX_glyglu[u]', -0.05 * peptMult
  'EX_glyleu[u]', -0.05 * peptMult
  'EX_glymet[u]', -0.05 * peptMult
  'EX_glyphe[u]', -0.05 * peptMult
  'EX_glypro[u]', -0.05 * peptMult
  'EX_glytyr[u]', -0.05 * peptMult
  'EX_metala[u]', -0.05 * peptMult
  'EX_butso3[u]', -0.05  * otherMult
  'EX_ethso3[u]', -0.05 * otherMult
  'EX_h2s[u]', -0.5 * otherMult
  'EX_hexs[u]', -0.05 * otherMult
  'EX_isetac[u]', -0.05 * otherMult
  'EX_Lcyst[u]', -0.05 * otherMult
  'EX_mso3[u]', -0.05 * otherMult
  'EX_ptrc[u]', -0.5 * otherMult
  'EX_so4[u]', -0.5 * otherMult
  'EX_spmd[u]', -0.5 * otherMult
  'EX_sulfac[u]', -0.05 * otherMult
  'EX_taur[u]', -0.05 * otherMult
  'EX_tsul[u]', -0.05 * otherMult
  'EX_4ahmmp[u]', -0.1 * otherMult
  'EX_5mthf[u]', -0.1 * otherMult
  'EX_adocbl[u]', -0.05 * otherMult
  'EX_btn[u]', -0.1 * otherMult
  'EX_coa[u]', -0.05 * otherMult
  'EX_dhpt[u]', -0.1 * otherMult
  'EX_dxyl5p[u]', -0.1 * otherMult
  'EX_fol[u]', -0.05 * otherMult
  'EX_nac[u]', -0.1 * otherMult
  'EX_nmn[u]', -0.05 * otherMult
  'EX_pdx5p[u]', -0.05 * otherMult
  'EX_pnto-R[u]', -0.1 * otherMult
  'EX_pydam[u]', -0.1 * otherMult
  'EX_pydx5p[u]', -0.1 * otherMult
  'EX_pydxn[u]', -0.1 * otherMult
  'EX_ribflv[u]', -0.05 * otherMult
  'EX_thf[u]', -0.05 * otherMult
  'EX_thm[u]', -0.05 * otherMult
};

rxns = rxnLbCell(:, 1);
lbs = cell2mat(rxnLbCell(:, 2));


if exist('removeRxns', 'var')
  modelOut = updateBounds(multiModel, rxns, lbs, removeRxns);
else
  modelOut = updateBounds(multiModel, rxns, lbs);
end



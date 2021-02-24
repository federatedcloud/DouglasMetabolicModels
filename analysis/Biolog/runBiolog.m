function biologResults = runBiolog(modelKey, modelMap)

minPlusMod = makeMultiModel({modelKey}, modelMap, 'biolog');

% First set 'EX_glc-D[u]' to 0, then set each of these to -0.05
minPlusMod = changeRxnBounds(minPlusMod, {'EX_glc-D[u]'}, 0, 'l');

biologRxns = {
  'EX_glypro[u]',
  'EX_ala-L[u]',
  'EX_arg-L[u]',
  'EX_asp-L[u]',
  'EX_asp-L[u]',
  'EX_glu-L[u]',
  'EX_his-L[u]',
  'EX_ser-D[u]',
  'EX_ser-L[u]',
  'EX_4abut[u]',
  'EX_ac[u]',
  'EX_acgala[u]',
  'EX_acgam[u]',
  'EX_akg[u]',
  'EX_cellb[u]',
  'EX_cit[u]',
  'EX_for[u]',
  'EX_fru[u]',
  'EX_fuc-L[u]',
  'EX_fuc-L[u]',
  'EX_gal[u]',
  'EX_glc-D[u]',
  'EX_glcn[u]',
  'EX_glyc[u]',
  'EX_lac-L[u]',
  'EX_lcts[u]',
  'EX_mal-D[u]',
  'EX_mal-L[u]',
  'EX_malt[u]',
  'EX_man[u]',
  'EX_melib[u]',
  'EX_mnl[u]',
  'EX_sbt-D[u]',
  'EX_succ[u]',
  'EX_sucr[u]',
  'EX_tre[u]',
  'EX_ins[u]',
  'EX_raffin[u]',
  'EX_pectin[u]',
  'EX_4hoxpac[u]',
  'EX_dextrin[u]',
  'EX_galur[u]',
  'EX_abt__D[u]',
  'EX_4ohbut[u]',
  'EX_mbdg[u]',
  'EX_inost[u]',
  'EX_salcn[u]',
  'EX_glcur[u]',
  'EX_g6p[u]',
  'EX_acac[u]',
  'EX_acmana[u]',
  'EX_f6p[u]',
  'EX_ppa[u]',
  'EX_rmn[u]',
  'EX_5oxpro[u]',
  'EX_quin[u]',
  'EX_stys[u]',
  'EX_acnam[u]',
  'EX_glcr[u]'
};

  function growth = growthForSource(mModel, rxn)
    growth = nan;
    if find(strcmp(mModel.rxns, rxn))
      mSub = changeRxnBounds(mModel, {rxn}, -0.05, 'l');
      sol = optimizeCbModel(mSub);
      growth = sol.obj;
    end
  end

biologResults = cellFlatMap(@(r) {r, growthForSource(minPlusMod, r)}, biologRxns);

end

function outRes = fixSCRes(inRes)
  outRes = inRes;
  if (inRes.GRmax < 1e-8 || ~contains(inRes.stat, 'optimal'))
    outRes.vBM = inRes.vBM * 0;
    outRes.BM = inRes.BM * 0;
    outRes.flux = inRes.flux * 0;
  end
end

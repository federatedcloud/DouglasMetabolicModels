function outRes = fixSCRes(inRes)
  outRes = inRes;
  bool1 = inRes.GRmax < 1e-8;
  bool2 = ~contains(inRes.stat, 'optimal');
  if (isempty(bool1) || isempty(bool2) || bool1 || bool2)
    outRes.vBM = inRes.vBM * 0;
    outRes.BM = inRes.BM * 0;
    outRes.flux = inRes.flux * 0;
  end
end

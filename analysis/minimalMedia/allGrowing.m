function notZero = allGrowing(simRes)
  notZero = all(simRes.res.BM) && (simRes.res.GRmax > 0);
end


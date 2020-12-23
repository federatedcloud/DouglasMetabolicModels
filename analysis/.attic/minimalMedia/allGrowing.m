function notZero = allGrowing(simRes)
  notZero = (~strcmp(simRes.res.stat, 'infeasible')) && all(simRes.res.vBM) && (simRes.res.GRmax > 0);
end

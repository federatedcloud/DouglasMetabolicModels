function displayScRes(res)
  disp("vBM = GRmax * BM");

  disp('vBM');
  disp(res.vBM);

  disp('GRmax');
  disp(res.GRmax)

  disp('BM');
  disp(res.BM);

  disp('sum(vBM)');
  disp(sum(res.vBM));

  disp('sum(BM)');
  disp(sum(res.BM));

end

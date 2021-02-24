% This analysis can be run by supplying the modelMap (stored
% in e.g. all_5.mat)
function runAnalysis(modelMap)
  % biologRes = cellFlatMap(@(sp) {sp, runBiolog(sp, allModelsMap)}, fields(allModelsMap));
  biologRes = cellFlatMap(@(sp) {sp, runBiolog(sp, modelMap)}, fields(modelMap));
  headerCell = [' '; cellFlatMap(@(tup) tup{1}, biologRes)];
  biologRxns = cellFlatMap(@(tup) tup{1}, biologRes{1}{2});
  biologCols = cellFlatMap(@(row) ...
    cellFlatMap(@(tup) tup{2}, row), cellFlatMap(@(tup) tup{2}, biologRes));

  header = strjoin(headerCell, ',');
  fid = fopen('_1.csv', 'wt');
  fprintf(fid, header);
  fprintf(fid, '\n');
  fclose(fid);

  writeCTable('_2.csv', biologRxns, biologCols{:});
  system('cat _1.csv _2.csv > biolog_sim.csv');
  system('rm _1.csv _2.csv');
end

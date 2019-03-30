function analysis = runAnalysis(modelMap, mediaType)

  scomRunner = makeCoopFluxSim(modelMap, mediaType);
  gitSha1 = currentGitSha;

  [fvalues, comparisons, memFun] = ...
  cellPowerSetFilter(scomRunner, @isNonZeroGrowth, keys(modelMap), @compareFluxes);
  analysis = struct;
  analysis.comparisons = comparisons;
  analysis.fvalues = fvalues;

  % Print out memFun to make sure it was used as expected.
  memFun
  memFun.stats

  gitSha2 = currentGitSha;
  assert(strcmp(gitSha1, gitSha2));

  timestamp = datestr(now,'mmmm-dd-yyyy-HH-MM');
  outDirectory = strjoin({mediaType, timestamp, gitSha1}, '_');
  system(strjoin({'mkdir -p', outDirectory}));
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    fName = strjoin({outDirectory, filesep, comp.label, '.csv'}, '');
    writetable(comp.table, fName);
  end
end

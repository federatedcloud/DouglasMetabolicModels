%
% multiModel - an already-created multi species model
%
% rxnList    - reactions to remove - this is the set of reactions
%              on which a power set analysis is performed.
%
function analysis = runAnalysis(multiModel, rxnList, tag)

  scomRunner = restrictUptakeFluxSim(multiModel, rxnList);
  gitSha1 = currentGitSha;
  [fvalues, comparisons, memFun] = ...
  cellPowerSetAllChildFilter( ...
    scomRunner, @allGrowing, rxnList, @compareFluxes, 'all' ...
  );
  analysis = struct;
  analysis.comparisons = comparisons;
  analysis.fvalues = fvalues;

  % Print out memFun to make sure it was used as expected.
  memFun
  if isfield(memFun, 'stats')
      memFun.stats
  else
      disp('no stats field for memFun!');
  end

  gitSha2 = currentGitSha;
  assert(strcmp(gitSha1, gitSha2));

  timestamp = datestr(now, 'mmmm-dd-yyyy-HH-MM');
  species = strjoin(multiModel.infoCom.spAbbr, '_');
  outFile = strjoin({tag, species, timestamp, gitSha1, '.csv' }, '_');
  numCmps = numel(comparisons);
  cellDataOut = {};
  for ii = 1:numCmps
    comp = comparisons{ii};
    if comp.minimal
      rxnListStr = strjoin(comp.minRxns, ', ');
      cellDataOut(end+1, :) = {numel(comp.minRxns), rxnListStr};
    end
  end
  if numel(cellDataOut) > 0
    cellDataOut = sortrows(cellDataOut);
    tableOut = cell2table(cellDataOut);
    writetable(tableOut, outFile);
  else
      disp('No minimal media found');
  end

end

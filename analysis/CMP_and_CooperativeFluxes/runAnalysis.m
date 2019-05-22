% Constructs all combinations of models and compares
% parent models with child models (model with 1 fewer species).
%
% function analysis = runAnalysis(modelMap, mediaType)
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
  % Summary Comparison analysis
  dfName = strjoin({outDirectory, filesep, 'differential', '.csv'}, '');
  dHeader = strjoin({ ...
    'Communities', 'Child Size', 'Parent Size', ...
    'Child_Lost_rxns', 'Parent_Gained_rxns', ...
    'CMP_Diff', 'CMP_EX_Diff', ...
    'CMP_Child', 'CMP_Parent', ...
    'CMP_EX_Child', 'CMP_EX_Parent'}, ',');
  fid = fopen(dfName, 'wt+');
  fprintf(fid, '%s\n', dHeader);
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    cmpDiff = comp.cmp.parent - comp.cmp.child;
    cmpExDiff = comp.cmpEx.parent - comp.cmpEx.child;
    childLost = strjoin(comp.childLostRxns, ';');
    parGained = strjoin(comp.parentGainedRxns, ';');
    fprintf(fid, '%s,%d,%d,%s,%s,%d,%d,%d,%d,%d,%d\n', ...
      comp.label, comp.size.child, comp.size.parent, ...
      childLost, parGained, ...
      cmpDiff, cmpExDiff, ...
      comp.cmp.child, comp.cmp.parent, ...
      comp.cmpEx.child, comp.cmpEx.parent);
  end
  fclose(fid);

  % Individual community analysis
  cmpMap = containers.Map();
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    % parent case
    cmpData = struct;
    cmpData.label = comp.parName;
    cmpData.size = comp.size.parent;
    cmpData.cmp = comp.cmp.parent;
    cmpData.cmpEx = comp.cmpEx.parent;
    cmpMap(cmpData.label) = cmpData;
    % child case
    cmpData = struct;
    cmpData.label = comp.childName;
    cmpData.size = comp.size.child;
    cmpData.cmp = comp.cmp.child;
    cmpData.cmpEx = comp.cmpEx.child;
    cmpMap(cmpData.label) = cmpData;
  end
  sfName = strjoin({outDirectory, filesep, 'individual', '.csv'}, '');
  sHeader = strjoin({'Community', '#species', 'CMP', 'CMP_EX'}, ',');
  fid = fopen(sfName, 'wt+');
  fprintf(fid, '%s\n', sHeader);
  commLabels = keys(cmpMap);
  for ii = 1:numel(commLabels)
    label = commLabels{ii};
    rec = cmpMap(label);
    fprintf(fid, '%s,%d,%d,%d\n', rec.label, rec.size, rec.cmp, rec.cmpEx);
  end
  fclose(fid);

  % Overlapping inputs analysis (individuals)
  oInpMap = containers.Map();
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    % parent case
    olapKeys = comp.overlappingInputs.parent.keys;
    for jj = 1:numel(olapKeys)
      orgKey = olapKeys{jj};
      olapInputs = comp.overlappingInputs.parent(orgKey);
      oInpData = struct;
      oInpData.label = comp.parName;
      oInpData.size = comp.size.parent;
      oInpData.org = olapInputs.org;
      oInpData.overlap = olapInputs.count;
      oInpData.degrees = olapInputs.list;
      oInKey = strjoin({oInpData.label,oInpData.org }, ';');
      oInpMap(oInKey) = oInpData;
    end
    % child case
    olapKeys = comp.overlappingInputs.child.keys;
    for jj = 1:numel(olapKeys)
      orgKey = olapKeys{jj};
      olapInputs = comp.overlappingInputs.child(orgKey);
      oInpData = struct;
      oInpData.label = comp.childName;
      oInpData.size = comp.size.child;
      oInpData.org = olapInputs.org;
      oInpData.overlap = olapInputs.count;
      oInpData.degrees = olapInputs.list;
      oInKey = strjoin({oInpData.label,oInpData.org }, ';');
      oInpMap(oInKey) = oInpData;
  end
  olfName = strjoin({outDirectory, filesep, 'influxOverlap', '.csv'}, '');
  olHeader = strjoin({'Community', '#species', 'Org', 'overlap', 'degree'}, ',');
  fid = fopen(olfName, 'wt+');
  fprintf(fid, '%s\n', olHeader);
  commLabels = keys(oInpMap);
  for ii = 1:numel(commLabels)
    label = commLabels{ii};
    rec = oInpMap(label);
    degStr = strjoin(                                      ...
      cellFlatMap(@(x) num2str(x), num2cell(rec.degrees)), ...
      ';');
    fprintf(fid, '%s,%d,%s,%d,%s\n', ...
      rec.label, rec.size, rec.org, rec.overlap, degStr);
  end
  fclose(fid);


end

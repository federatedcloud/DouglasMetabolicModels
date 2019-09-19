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
  mfName = strjoin({outDirectory, filesep, 'analysis.mat'}, '');
  save(mfName, 'analysis');
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    fName = strjoin({outDirectory, filesep, comp.label, '.csv'}, '');
    writetable(comp.table, fName);
  end
  % Summary Comparison analysis
  dfName = strjoin({outDirectory, filesep, 'differential', '.csv'}, '');

  cmpDiff = @(c) c.cmp.parent - c.cmp.child;

  cmpInExDiff = @(c) c.cmpInEx.parent - c.cmpInEx.child;
  cmpInTrDiff = @(c) c.cmpInTr.parent - c.cmpInTr.child;

  cmpOutExDiff = @(c) c.cmpOutEx.parent - c.cmpOutEx.child;
  cmpOutTrDiff = @(c) c.cmpOutTr.parent - c.cmpOutTr.child;

  cmpDiffUq = @(c) c.cmpUniq.parent - c.cmpUniq.child;

  cmpInExDiffUq = @(c) c.cmpInExUniq.parent - c.cmpInExUniq.child;
  cmpInTrDiffUq = @(c) c.cmpInTrUniq.parent - c.cmpInTrUniq.child;

  cmpOutExDiffUq = @(c) c.cmpOutExUniq.parent - c.cmpOutExUniq.child;
  cmpOutTrDiffUq = @(c) c.cmpOutTrUniq.parent - c.cmpOutTrUniq.child;

  childLost = @(c) strjoin(c.childLostRxns, ';');
  parGained = @(c) strjoin(c.parentGainedRxns, ';');

  cmpDiffFields = { ...
    'Communities', '%s', @(c) c.label; ...
    'Child Size', '%d', @(c) c.size.child; ...
    'Parent Size', '%d', @(c) c.size.parent; ...
    'Child_Lost_rxns', '%s', childLost; ...
    'Parent_Gained_rxns', '%s', parGained; ...
    'CMP_Diff', '%d', cmpDiff; ...
    ...
    'CMP_IN_EX_Diff', '%d', cmpInExDiff; ...
    'CMP_IN_TR_Diff', '%d', cmpInTrDiff; ...
    ...
    'CMP_OUT_EX_Diff', '%d', cmpOutExDiff; ...
    'CMP_OUT_TR_Diff', '%d', cmpOutTrDiff; ...
    ...
    'CMP_Diff_Uniq', '%d', cmpDiffUq; ...
    ...
    'CMP_IN_EX_Diff_Uniq', '%d', cmpInExDiffUq; ...
    'CMP_IN_TR_Diff_Uniq', '%d', cmpInTrDiffUq; ...
    ...
    'CMP_OUT_EX_Diff_Uniq', '%d', cmpOutExDiffUq; ...
    'CMP_OUT_TR_Diff_Uniq', '%d', cmpOutTrDiffUq; ...
    ...
    'CMP_Child', '%d', @(c) c.cmp.child; ...
    'CMP_Parent', '%d', @(c) c.cmp.parent; ...
    'CMP_Child_Uniq', '%d', @(c) c.cmpUniq.child; ...
    'CMP_Parent_Uniq', '%d', @(c) c.cmpUniq.parent; ...
    ...
    'CMP_IN_EX_Child', '%d', @(c) c.cmpInEx.child; ...
    'CMP_IN_EX_Parent', '%d', @(c) c.cmpInEx.parent; ...
    'CMP_IN_EX_Child_uniq', '%d', @(c) c.cmpInExUniq.child; ...
    'CMP_IN_EX_Parent_Uniq', '%d', @(c) c.cmpInExUniq.parent; ...
    'CMP_IN_TR_Child', '%d', @(c) c.cmpInTr.child; ...
    'CMP_IN_TR_Parent', '%d', @(c) c.cmpInTr.parent; ...
    'CMP_IN_TR_Child_uniq', '%d', @(c) c.cmpInTrUniq.child; ...
    'CMP_IN_TR_Parent_Uniq' '%d', @(c) c.cmpInTrUniq.parent; ...
    ...
    'CMP_OUT_EX_Child', '%d', @(c) c.cmpOutEx.child; ...
    'CMP_OUT_EX_Parent', '%d', @(c) c.cmpOutEx.parent; ...
    'CMP_OUT_EX_Child_uniq', '%d', @(c) c.cmpOutExUniq.child; ...
    'CMP_OUT_EX_Parent_Uniq', '%d', @(c) c.cmpOutExUniq.parent; ...
    'CMP_OUT_TR_Child', '%d', @(c) c.cmpOutTr.child; ...
    'CMP_OUT_TR_Parent', '%d', @(c) c.cmpOutTr.parent; ...
    'CMP_OUT_TR_Child_uniq', '%d', @(c) c.cmpOutTrUniq.child; ...
    'CMP_OUT_TR_Parent_Uniq' '%d', @(c) c.cmpOutTrUniq.parent; ...
  };
  dHeader = strjoin(cmpDiffFields(:, 1), ',');
  fid = fopen(dfName, 'wt+');
  fprintf(fid, '%s\n', dHeader);
  cmpDiffFmtSpec = strcat(strjoin(cmpDiffFields(:, 2), ','), '\n');
  cmpDiffFieldVals = @(comp) cellFlatMap(@(ap) ap(comp), cmpDiffFields(:,3));
  for ii = 1:numel(comparisons)
    comp = comparisons{ii};
    cfVals = cmpDiffFieldVals(comp);
    fprintf(fid, cmpDiffFmtSpec, cfVals{:});
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

    cmpData.cmpInEx = comp.cmpInEx.parent;
    cmpData.cmpInTr = comp.cmpInTr.parent;

    cmpData.cmpOutEx = comp.cmpOutEx.parent;
    cmpData.cmpOutTr = comp.cmpOutTr.parent;

    cmpData.cmpUq = comp.cmpUniq.parent;

    cmpData.cmpInExUq = comp.cmpInExUniq.parent;
    cmpData.cmpInTrUq = comp.cmpInTrUniq.parent;

    cmpData.cmpOutExUq = comp.cmpOutExUniq.parent;
    cmpData.cmpOutTrUq = comp.cmpOutTrUniq.parent;

    cmpMap(cmpData.label) = cmpData;
    % child case
    cmpData = struct;
    cmpData.label = comp.childName;
    cmpData.size = comp.size.child;
    cmpData.cmp = comp.cmp.child;

    cmpData.cmpInEx = comp.cmpInEx.child;
    cmpData.cmpInTr = comp.cmpInTr.child;

    cmpData.cmpOutEx = comp.cmpOutEx.child;
    cmpData.cmpOutTr = comp.cmpOutTr.child;

    cmpData.cmpUq = comp.cmpUniq.child;

    cmpData.cmpInExUq = comp.cmpInExUniq.child;
    cmpData.cmpInTrUq = comp.cmpInTrUniq.child;

    cmpData.cmpOutExUq = comp.cmpOutExUniq.child;
    cmpData.cmpOutTrUq = comp.cmpOutTrUniq.child;

    cmpMap(cmpData.label) = cmpData;
  end
  sfName = strjoin({outDirectory, filesep, 'individual', '.csv'}, '');
  cmpFields = { ...
    'Community', '%s', @(r) r.label; ...
    '#species', '%d', @(r) r.size; ...
    'CMP', '%d', @(r) r.cmp; ...
    ...
    'CMP_IN_EX', '%d', @(r) r.cmpInEx; ...
    'CMP_IN_TR', '%d', @(r) r.cmpInTr; ...
    ...
    'CMP_OUT_EX', '%d', @(r) r.cmpOutEx; ...
    'CMP_OUT_TR', '%d', @(r) r.cmpOutTr; ...
    ...
    'CMP_Uniq', '%d', @(r) r.cmpUq; ...
    ...
    'CMP_IN_EX_Uniq', '%d', @(r) r.cmpInExUq; ...
    'CMP_IN_TR_Uniq', '%d', @(r) r.cmpInTrUq; ...
    ...
    'CMP_OUT_EX_Uniq', '%d', @(r) r.cmpOutExUq; ...
    'CMP_OUT_TR_Uniq', '%d', @(r) r.cmpOutTrUq; ...
  };
  sHeader = strjoin(cmpFields(:, 1), ',');
  fid = fopen(sfName, 'wt+');
  fprintf(fid, '%s\n', sHeader);
  cmpFmtSpec = strcat(strjoin(cmpFields(:, 2), ','), '\n');
  cmpFieldVals = @(rec) cellFlatMap(@(ap) ap(rec), cmpFields(:,3));
  commLabels = keys(cmpMap);
  for ii = 1:numel(commLabels)
    label = commLabels{ii};
    rec = cmpMap(label);
    cfVals = cmpFieldVals(rec);
    fprintf(fid, cmpFmtSpec, cfVals{:});
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

  % Write heatmaps
  heatMapTbls = genHeatMapTables(analysis);
  exHeatFName = strjoin({outDirectory, filesep, 'heatmap_exchange.csv'}, '');
  writetable(cell2table(heatMapTbls.exchange), exHeatFName, 'WriteVariableNames', false);
  transHeatFName = strjoin({outDirectory, filesep, 'heatmap_trans.csv'}, '');
  writetable(cell2table(heatMapTbls.trans), transHeatFName, 'WriteVariableNames', false);

end

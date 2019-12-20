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

  function genCMPFiles(oLabel)
    getCmp = @(c) c.CMP;

    if ~isempty(oLabel)
      getCmp = @(c) c.CMPnoInorg;
    end
    getCmpF = @(c,f) getfield(getCmp(c), f);

    % Summary Comparison analysis
    dfName = strjoin({outDirectory, filesep, 'differential', oLabel, '.csv'}, '');

    cmpDiff = @(c) getCmpF(c, 'parent') - getCmpF(c, 'child');

    cmpInExDiff = @(c) getCmpF(c, 'InExParent') - getCmpF(c, 'InExChild');
    cmpInTrDiff = @(c) getCmpF(c, 'InTrParent') - getCmpF(c, 'InTrChild');

    cmpOutExDiff = @(c) getCmpF(c, 'OutExParent') - getCmpF(c, 'OutExChild');
    cmpOutTrDiff = @(c) getCmpF(c, 'OutTrParent') - getCmpF(c, 'OutTrChild');

    cmpDiffUq = @(c) getCmpF(c, 'UniqParent') - getCmpF(c, 'UniqChild');

    cmpInExDiffUq = @(c) getCmpF(c, 'InExUniqParent') - getCmpF(c, 'InExUniqChild');
    cmpInTrDiffUq = @(c) getCmpF(c, 'InTrUniqParent') - getCmpF(c, 'InTrUniqChild');

    cmpOutExDiffUq = @(c) getCmpF(c, 'OutExUniqParent') - getCmpF(c, 'OutExUniqChild');
    cmpOutTrDiffUq = @(c) getCmpF(c, 'OutTrUniqParent') - getCmpF(c, 'OutTrUniqChild');

    childLost = @(c) strjoin(getCmpF(c, 'childLostRxns'), ';');
    parGained = @(c) strjoin(getCmpF(c, 'parentGainedRxns'), ';');

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
      'CMP_Child', '%d', @(c) getCmpF(c, 'child'); ...
      'CMP_Parent', '%d', @(c) getCmpF(c, 'parent'); ...
      'CMP_Child_Uniq', '%d', @(c) getCmpF(c, 'UniqChild'); ...
      'CMP_Parent_Uniq', '%d', @(c) getCmpF(c, 'UniqParent'); ...
      ...
      'CMP_IN_EX_Child', '%d', @(c) getCmpF(c, 'InExChild'); ...
      'CMP_IN_EX_Parent', '%d', @(c) getCmpF(c, 'InExParent'); ...
      'CMP_IN_EX_Child_uniq', '%d', @(c) getCmpF(c, 'InExUniqChild'); ...
      'CMP_IN_EX_Parent_Uniq', '%d', @(c) getCmpF(c, 'InExUniqParent'); ...
      'CMP_IN_TR_Child', '%d', @(c) getCmpF(c, 'InTrChild'); ...
      'CMP_IN_TR_Parent', '%d', @(c) getCmpF(c, 'InTrParent'); ...
      'CMP_IN_TR_Child_uniq', '%d', @(c) getCmpF(c, 'InTrUniqChild'); ...
      'CMP_IN_TR_Parent_Uniq' '%d', @(c) getCmpF(c, 'InTrUniqParent'); ...
      ...
      'CMP_OUT_EX_Child', '%d', @(c) getCmpF(c, 'OutExChild'); ...
      'CMP_OUT_EX_Parent', '%d', @(c) getCmpF(c, 'OutExParent'); ...
      'CMP_OUT_EX_Child_uniq', '%d', @(c) getCmpF(c, 'OutExUniqChild'); ...
      'CMP_OUT_EX_Parent_Uniq', '%d', @(c) getCmpF(c, 'OutExUniqParent'); ...
      'CMP_OUT_TR_Child', '%d', @(c) getCmpF(c, 'OutTrChild'); ...
      'CMP_OUT_TR_Parent', '%d', @(c) getCmpF(c, 'OutTrParent'); ...
      'CMP_OUT_TR_Child_uniq', '%d', @(c) getCmpF(c, 'OutTrUniqChild'); ...
      'CMP_OUT_TR_Parent_Uniq' '%d', @(c) getCmpF(c, 'OutTrUniqParent'); ...
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
    for i3 = 1:numel(comparisons)
      comp = comparisons{i3};
      % parent case
      cmpData = struct;
      cmpData.label = comp.parName;
      cmpData.size = comp.size.parent;
      cmpData.cmp = getCmpF(comp, 'parent');

      cmpData.cmpInEx = getCmpF(comp, 'InExParent');
      cmpData.cmpInTr = getCmpF(comp, 'InTrParent');

      cmpData.cmpOutEx = getCmpF(comp, 'OutExParent');
      cmpData.cmpOutTr = getCmpF(comp, 'OutTrParent');

      cmpData.cmpUq = getCmpF(comp, 'UniqParent');

      cmpData.cmpInExUq = getCmpF(comp, 'InExUniqParent');
      cmpData.cmpInTrUq = getCmpF(comp, 'InTrUniqParent');

      cmpData.cmpOutExUq = getCmpF(comp, 'OutExUniqParent');
      cmpData.cmpOutTrUq = getCmpF(comp, 'OutTrUniqParent');

      cmpMap(cmpData.label) = cmpData;
      % child case
      cmpData = struct;
      cmpData.label = comp.childName;
      cmpData.size = comp.size.child;
      cmpData.cmp = getCmpF(comp, 'child');

      cmpData.cmpInEx = getCmpF(comp, 'InExChild');
      cmpData.cmpInTr = getCmpF(comp, 'InTrChild');

      cmpData.cmpOutEx = getCmpF(comp, 'OutExChild');
      cmpData.cmpOutTr = getCmpF(comp, 'OutTrChild');

      cmpData.cmpUq = getCmpF(comp, 'UniqChild');

      cmpData.cmpInExUq = getCmpF(comp, 'InExUniqChild');
      cmpData.cmpInTrUq = getCmpF(comp, 'InTrUniqChild');

      cmpData.cmpOutExUq = getCmpF(comp, 'OutExUniqChild');
      cmpData.cmpOutTrUq = getCmpF(comp, 'OutTrUniqChild');

      cmpMap(cmpData.label) = cmpData;
    end
    sfName = strjoin({outDirectory, filesep, 'individual', oLabel, '.csv'}, '');
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
    for i4 = 1:numel(commLabels)
      label = commLabels{i4};
      rec = cmpMap(label);
      cfVals = cmpFieldVals(rec);
      fprintf(fid, cmpFmtSpec, cfVals{:});
    end
    fclose(fid);
  end
  genCMPFiles('');
  genCMPFiles('_NoInorganicIons');

  % Overlapping transport analysis (individuals)
  function genOverlapFiles(oLabel)
    oTrMap = containers.Map();
    for i5 = 1:numel(comparisons)
      comp = comparisons{i5};
      compOverlapping = comp.overlappingTr;
      if ~isempty(oLabel)
        compOverlapping = comp.overlappingTrNoInorg;
      end
      % parent case
      olapKeys = compOverlapping.parent.keys;
      for jj = 1:numel(olapKeys)
        orgKey = olapKeys{jj};
        olapIO = compOverlapping.parent(orgKey);
        oTrData = struct;
        oTrData.label = comp.parName;
        oTrData.size = comp.size.parent;
        oTrData.org = olapIO.org;
        oTrData.overlapIn = olapIO.countIn;
        oTrData.degreesIn = olapIO.listIn;
        oTrData.overlapOut = olapIO.countOut;
        oTrData.degreesOut = olapIO.listOut;
        oTrData.inFluxSum = olapIO.inFluxSum;
        oTrData.outFluxSum = olapIO.outFluxSum;
        oTrData.inFluxCount = olapIO.inFluxCount;
        oTrData.outFluxCount = olapIO.outFluxCount;
        oTrData.inFluxRxns = olapIO.inFluxRxns;
        oTrData.outFluxRxns = olapIO.outFluxRxns;
        oTrData.inRxnsOverlap = olapIO.inRxnsOverlap;
        oTrData.outRxnsOverlap = olapIO.outRxnsOverlap;
        oTrKey = strjoin({oTrData.label,oTrData.org }, ';');
        oTrMap(oTrKey) = oTrData;
      end
      % child case
      olapKeys = compOverlapping.child.keys;
      for jj = 1:numel(olapKeys)
        orgKey = olapKeys{jj};
        olapIO = compOverlapping.child(orgKey);
        oTrData = struct;
        oTrData.label = comp.childName;
        oTrData.size = comp.size.child;
        oTrData.org = olapIO.org;
        oTrData.overlapIn = olapIO.countIn;
        oTrData.degreesIn = olapIO.listIn;
        oTrData.overlapOut = olapIO.countOut;
        oTrData.degreesOut = olapIO.listOut;
        oTrData.inFluxSum = olapIO.inFluxSum;
        oTrData.outFluxSum = olapIO.outFluxSum;
        oTrData.inFluxCount = olapIO.inFluxCount;
        oTrData.outFluxCount = olapIO.outFluxCount;
        oTrData.inFluxRxns = olapIO.inFluxRxns;
        oTrData.outFluxRxns = olapIO.outFluxRxns;
        oTrData.inRxnsOverlap = olapIO.inRxnsOverlap;
        oTrData.outRxnsOverlap = olapIO.outRxnsOverlap;
        oTrKey = strjoin({oTrData.label,oTrData.org }, ';');
        oTrMap(oTrKey) = oTrData;
      end
    end
    olfName = strjoin({outDirectory, filesep, 'overlap', oLabel, '.csv'}, '');
    olHeader = strjoin({'Community', '#species', 'Org', 'overlapIn', 'degreeIn', ...
                        'overlapOut', 'degreeOut', 'inFluxSum', 'outFluxSum',   ...
                        'inFluxCount', 'outFluxCount', 'inRxns', 'outRxns', ...
                        'inRxnsOverlap', 'outRxnsOverlap'} , ',');
    fid = fopen(olfName, 'wt+');
    fprintf(fid, '%s\n', olHeader);
    commLabels = keys(oTrMap);
    for i5 = 1:numel(commLabels)
      label = commLabels{i5};
      rec = oTrMap(label);
      degInStr = strjoin(                                      ...
        cellFlatMap(@(x) num2str(x), num2cell(rec.degreesIn)), ...
        ';');
      degOutStr = strjoin(                                      ...
        cellFlatMap(@(x) num2str(x), num2cell(rec.degreesOut)), ...
        ';');
      inRxnStr = strjoin(rec.inFluxRxns, ';');
      outRxnStr = strjoin(rec.outFluxRxns, ';');
      inRxnOlapStr = strjoin(rec.inRxnsOverlap, ';');
      outRxnOlapStr = strjoin(rec.outRxnsOverlap, ';');
      fprintf(fid, '%s,%d,%s,%d,%s,%d,%s,%d,%d,%d,%d,%s,%s,%s,%s\n',    ...
              rec.label, rec.size, rec.org, rec.overlapIn, degInStr,    ...
              rec.overlapOut, degOutStr, rec.inFluxSum, rec.outFluxSum, ...
              rec.inFluxCount, rec.outFluxCount, inRxnStr, outRxnStr,   ...
              inRxnOlapStr, outRxnOlapStr                               ...
             );
    end
    fclose(fid);
  end
  genOverlapFiles('');
  genOverlapFiles('_NoInorganicIons');

  % Write heatmaps
  heatMapTbls = genHeatMapTables(analysis);
  writeHeatMapTables(heatMapTbls, outDirectory);
end

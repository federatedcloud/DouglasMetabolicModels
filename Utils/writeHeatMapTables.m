% For testing call like: writeHeatMapTables(tableTestMin, pwd())
function writeHeatMapTables(heatMapTbls, outDir)
  heatMapTblKeys = keys(heatMapTbls);
  for i3 = 1:numel(heatMapTblKeys)
    tblKey = heatMapTblKeys{i3};
    heatFName = strjoin({outDir, filesep, 'heatmap_', tblKey, '.csv'}, '');
    writetable(cell2table(heatMapTbls(tblKey)), heatFName, 'WriteVariableNames', false);
  end
end

function groupMap = readRxnGroups(rxnGroupFile)
  tbl = readtable(rxnGroupFile);
  tblKeyCol = 'metabolicGroup_pathway';
  tblValCol = 'listOfReactionIds';
  groupMap = containers.Map();

  for ii = 1:height(tbl)
    gKey = tbl{ii, tblKeyCol}{1};
    gVal = splitString(tbl{ii, tblValCol}{1}, ';');
    groupMap(gKey) = gVal;
  end

end

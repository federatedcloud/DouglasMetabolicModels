% A simple functional combinator style filter; preservers original container type:
%
% function [out, indices] = filter1d(pred, inList)
function [out, indices] = filter1d(pred, inList)
  cellList = {};
  if ~(strcmp(class(inList), 'cell'))
    cellList = num2cell(inList);
    cellList = cellList(:);
  else
    cellList = inList(:);
  end

  predVals = cell2mat(cellfun(pred, cellList, 'UniformOutput', false));
  outCell = cellList(predVals);
  indices = find(predVals);

  out = outCell;
  if ~(strcmp(class(inList), 'cell'))
    out = cell2mat(outCell);
  end
end

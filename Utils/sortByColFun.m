function sorted = sortByColFun(sfun, cellIn, colIn)
  cellSz = size(cellIn);

  assert(numel(cellSz) < 3);

  col = 1;
  if nargin > 2
    col = colIn;
  end

  tmpCell = cellIn;
  sortVals = cellFlatMap(sfun, cellIn(:, col));
  tmpCell(:, end+1) = sortVals;

  sorted = sortrows(tmpCell, cellSz(2)+1);
  sorted(:, cellSz(2)+1) = [];
end

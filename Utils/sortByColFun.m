% Wrapper around sortrows to support sorting by functions on columns
% TODO: support descend/ascend - currently uses default ascend.
%
% Hint - use an "identity function", e.g. @(x) x, to sort alphanumerically
%        on a column
%
% Can sort single cell "vectors":
%
% >> tmpStrs = {'asf', 'asd', '123', 'xyz', 'za', 'ab', 'aa', 'aac'}
% 
% tmpStrs =
% 
%   1×8 cell array
% 
%     'asf'    'asd'    '123'    'xyz'    'za'    'ab'    'aa'    'aac'
% 
% >> sortByColFun({@(x) numel(x), @(x) x}, [1, 1], tmpStrs)
% 
% ans =
% 
%   1×8 cell array
% 
%     'aa'    'ab'    'za'    '123'    'aac'    'asd'    'asf'    'xyz'
% 
% 
%
%
%  Can also use with multiple columns: here we sort the food type first by
%  length then by alphanumeric sorting (both are on column 2)
%
% >> C = {5 'cereal' 110 'C+'; 12 'pizza' 140 'B';  23 'salmon' 367 'A'; 2 'cookies' 160 'D'}
% 
% C =
% 
%   4×4 cell array
% 
%     [ 5]    'cereal'     [110]    'C+'
%     [12]    'pizza'      [140]    'B' 
%     [23]    'salmon'     [367]    'A' 
%     [ 2]    'cookies'    [160]    'D' 
% 
% >> sortByColFun({@(x) numel(x), @(x) x}, [2 2], C)
% 
% ans =
% 
%   4×4 cell array
% 
%     [12]    'pizza'      [140]    'B' 
%     [ 5]    'cereal'     [110]    'C+'
%     [23]    'salmon'     [367]    'A' 
%     [ 2]    'cookies'    [160]    'D' 
% 
% 

function sorted = sortByColFun(sfuns, colsIn, cellIn)
  cellSz = size(cellIn);
  trans = false;
  if (cellSz(1) == 1)
    cellIn = cellIn';
    cellSz = size(cellIn);
    trans = true;
  end
  nCols = cellSz(2);

  assert(numel(cellSz) < 3);
  assert(numel(sfuns) == numel(colsIn));

  tmpCell = cellIn;

  nFuns = numel(sfuns);
  for ii = 1:nFuns
    sfun = sfuns{ii};
    col = colsIn(ii);

    sortVals = cellFlatMap(sfun, cellIn(:, col));
    tmpCell(:, end+1) = sortVals;
  end

  rowsToSort = (nCols+1):(nCols+nFuns);
  sorted = sortrows(tmpCell, rowsToSort);
  sorted(:, rowsToSort) = [];
  if trans
    sorted = sorted';
  end
end

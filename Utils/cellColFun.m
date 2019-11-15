% Takes a 2d cell array and performs a numeric function over the numeric values present
% in each column of the input cell array.
function summary = cellColFun(colFun, inCell)
  cellDims = size(inCell);
  nCols = cellDims(2);
  summary = cell(1, nCols);
  for ii = 1:nCols
    cellCol = inCell(:, ii);
    colNums = cell2mat(filter1d(@(x) length(x) > 0 && isnumeric(x), cellCol));
    result = num2cell(colFun(colNums));
    if sum(size(result)) == 0
      summary(ii) = {[]};
    else
      summary(ii) = result;
    end
  end
end

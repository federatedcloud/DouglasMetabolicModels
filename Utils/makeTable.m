function makeTable(varargin)
% First argument is filename for output
% All other arguments are cell arrays for vectors,
% and are printed as columns in an output table.
  assert(nargin > 1)
  outFileName = varargin{1};
  assert(strcmp(class(outFileName), 'char'));
  if nargin > 2
    numRows = numel(varargin{2});
    for ii = 3:nargin
      assert(numel(varargin{ii}) == numRows);
    end
  end
  cellForTable = cell(numRows, nargin-1);
  for ii = 2:nargin
    if ~(strcmp(class(varargin{ii}), 'cell'))
      % Attempt to convert it:
      varargin{ii} = num2cell(varargin{ii});
    end
    [cellForTable{:,ii-1}] = varargin{ii}{:};
  end
  tableOut = cell2table(cellForTable);
  writetable(tableOut, outFileName);
end

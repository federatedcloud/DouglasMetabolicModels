function tbl = makeTable(varargin)
% See writeCTable to write directly to a file.
% All arguments are cell arrays or vectors,
% and are printed as columns in an output table.
  assert(nargin > 0)
  if nargin > 1
    numRows = numel(varargin{1});
    for ii = 2:nargin
      assert(numel(varargin{ii}) == numRows);
    end
  end
  cellForTable = cell(numRows, nargin);
  for ii = 1:nargin
    if ~(strcmp(class(varargin{ii}), 'cell'))
      % Attempt to convert it:
      varargin{ii} = num2cell(varargin{ii});
    end
    [cellForTable{:,ii}] = varargin{ii}{:};
  end
  tbl = cell2table(cellForTable);
end

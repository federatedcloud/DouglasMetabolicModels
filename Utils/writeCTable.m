% First argument is filename for output
% All other arguments are cell arrays or vectors,
% and are printed as columns in an output table.
function writeCTable(varargin)
  assert(strcmp(class(varargin{1}), 'char'));
  tbl = makeTable(varargin{2:end});
  writetable(tbl, varargin{1}, 'WriteVariableNames', false);
end

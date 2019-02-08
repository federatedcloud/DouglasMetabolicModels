function out = cellFlatMap(fun, carray)
  out = cellfun(fun, carray, 'UniformOutput',false);

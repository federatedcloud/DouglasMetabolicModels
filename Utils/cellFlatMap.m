function out = cellFlatMap(fun, carray)
  cellfun(fun, carray, 'UniformOutput',false);

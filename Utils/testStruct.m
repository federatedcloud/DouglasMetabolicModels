function allOut = testStruct(someNames)
  allOut = struct;
  for ii = 1:100
    curName = someNames{mod(ii, length(someNames)) + 1};
    subStruct  = struct(       ...
      'foo', ii,               ...
      'bar', {{ii, ii+1, ii+2}}  ...
      );
    allOut = setfield(allOut, curName, subStruct);
  end
end

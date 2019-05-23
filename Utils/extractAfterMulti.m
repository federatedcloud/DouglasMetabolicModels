function strOut = extractAfterMulti(str, strStarts)
  if strcmp(class(strStarts), 'char')
    disp('Use extractAfter for char patterns instead of  extractAfterMulti');
  end
  assert(strcmp(class(strStarts), 'cell'));
  strOut = str;
  for ii=1:numel(strStarts)
    ss = strStarts{ii};
    if startsWith(str, ss)
      strOut = extractAfter(str, ss);
      break;
    end
  end
end

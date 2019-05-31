function entry = cellHead(contIn)
  if strcmp(class(contIn), 'cell')
    entry = contIn{1};
  else
    entry = contIn(1);
  end
end

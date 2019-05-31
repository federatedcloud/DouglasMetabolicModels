function retVal = getOrEmpty(getFun)
  retVal = cell(0);
  try
    retVal = getFun();
  catch
  end
end

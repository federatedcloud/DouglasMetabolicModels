function rxnOut = prependIforEX(rxnIn)
  rxnOut = rxnIn;
  if startsWith(rxnIn, 'EX')
    rxnOut = strjoin({'I', rxnIn}, '')
  end
end

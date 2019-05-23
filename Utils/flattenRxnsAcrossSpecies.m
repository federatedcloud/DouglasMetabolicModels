% Removes the organism-specific prefix from reactions in a

function rxnsWOprefix = flattenRxnsAcrossSpecies(rxns, multiModel)
  nRxns = numel(rxns);
  choppedRxns = cell(1, nRxns);
  for ii = 1:nRxns
    rxn = rxns{ii};
    choppedRxns{ii} = extractAfterMulti(rxn, multiModel.infoCom.spAbbr);
  end
  rxnsWOprefix = unique(choppedRxns);
end

function modelOut = updateBounds(multiModel, rxns, lbs, removeRxns)
  modelOut = multiModel;
  rxnLbMap = containers.Map(rxns, lbs);
  if exist('removeRxns', 'var')
    rxnLbMap.remove(removeRxns);
  end

  excIDs = findExcIDs(modelOut);
  modelOut.lb(excIDs) = 0;
  for rxn = keys(rxnLbMap)
    rxn = char(rxn);
    rxnIdx = find(strcmp(rxn, modelOut.rxns));
    if numel(rxnIdx) ~= 1
      warning(strjoin({'No reaction ', rxn, ' in model' }, ''));
    end
    modelOut.lb(rxnIdx) = rxnLbMap(rxn);
  end
end

function model_out = restoreExcBounds(multiModel, modelList)
% multiModel is the multiple species model with the incorrect exchange bounds
% modelList is an array of the original individual models that make up the
% multiple species model
%
% Authors: Cindy Wu, Brandon Barker
%
excIDs = findExcIDs(multiModel);
endChar = '(';
for exc = excIDs
    bounds = [];
    for modelIdx = 1:length(modelList)
        idxs = findExcIDs(modelList{modelIdx});
        rxns = modelList{modelIdx}.rxns(idxs);
        for rxnIdx=1:length(rxns)
            rxn = rxns(rxnIdx);
            rxn = regexprep(rxn, '_e$','');
            if contains(rxn, endChar)
                rxn = strcat(extractBefore(rxn, endChar), '[u]');
            end
            if ~contains(rxn, '[u]')
                rxn = strcat(rxn, '[u]');
            end
            rxns(rxnIdx) = rxn;
        end
        match = find(strcmp(rxns, multiModel.rxns(exc)));
        if ~isempty(match)
            bounds = modelList{modelIdx}.lb(idxs(match));
            multiModel = changeRxnBounds(multiModel, multiModel.rxns(exc), min(bounds), 'l');
        else
          % TODO: need to check all input models in batch before warning
          % TODO: related, can convert this to something with less loops and more cellfun
          % TODO: to perhaps imrpove performance
          % disp(strcat("Couldn't find a reaction for ", multiModel.rxns(exc)));
        end
    end
end
model_out = multiModel;
end

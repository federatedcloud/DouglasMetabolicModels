model_1in = load('iNA855.mat')
model_1= preProcessModel(model_1in.model)

model_2in = load('iNA854_argKO.mat')
model_3in = load('iNA854_hisKO.mat')
model_3= preProcessModel(model_3in.model)


% to avoid errors associated with PubChemID column in the createMultipleSpeciesModel 
model_2.metPubChemID = num2cell(model_2.metPubChemID)
model_3.metPubChemID = num2cell(model_3.metPubChemID)
 
multi_model =  createMultipleSpeciesModel({model_1; model_3}, {'Org1'; 'Org2'})
[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'Org1'; 'Org2'});

disp( multi_model.infoCom)
disp( multi_model.indCom)

rxnBiomass = strcat({'Org1'; 'Org2'}, 'LP_GROW');  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;
multi_model.csense = char('E' * ones(1,numel(multi_model.mets)));  % correct the csense

function modelOut = preProcessModel(modelIn)
    % convert the compartment format from e.g., '_c' to '[c]'
    modelIn.mets = regexprep(modelIn.mets, '_([^_]+)$', '\[$1\]');
    % make all empty cells in cell arrays to be empty string
    fieldToBeCellStr = {'metFormulas'; 'genes'; 'grRules'; 'metNames'; 'rxnNames'; 'subSystems'};
    for j = 1:numel(fieldToBeCellStr)
        modelIn.(fieldToBeCellStr{j})(cellfun(@isempty, modelIn.(fieldToBeCellStr{j}))) = {''};
    end
    modelOut = modelIn;

end
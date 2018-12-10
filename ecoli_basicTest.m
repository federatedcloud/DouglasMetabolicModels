model_1 = readCbModel('iAF1260.mat');
%model_1 = readCbModel('ecoli_1.2_cobra.xml');


% convert the compartment format from e.g., '_c' to '[c]'
model_1.mets = regexprep(model_1.mets, '_([^_]+)$', '\[$1\]');
% make all empty cells in cell arrays to be empty string
fieldToBeCellStr = {'metFormulas'; 'genes'; 'grRules'; 'metNames'; 'rxnNames'; 'subSystems'};
for j = 1:numel(fieldToBeCellStr)
    if isfield(model_1, fieldToBeCellStr{j})
        model_1.(fieldToBeCellStr{j})(cellfun(@isempty, model_1.(fieldToBeCellStr{j}))) = {''};
    end
end

growth = "growth";
if model_1.rxns{find(model_1.c)} == ""
    model_1.rxns{find(model_1.c)} = 'growth';
else 
    growth = model_1.rxns(find(model_1.c));
end

model_2 = model_1;
model_3 = model_1;

% to avoid errors associated with PubChemID column in the createMultipleSpeciesModel 
%model_2.metPubChemID = num2cell(model_2.metPubChemID)
%model_3.metPubChemID = num2cell(model_3.metPubChemID)
 

multi_model =  createMultipleSpeciesModel({model_1; model_3}, {'Org1'; 'Org2'});
[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'Org1'; 'Org2'});

multi_model.csense = char('E' * ones(1,numel(multi_model.mets)));  % correct the csense

disp( multi_model.infoCom)
disp( multi_model.indCom)

rxnBiomass = strcat({'Org1'; 'Org2'}, growth);  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;

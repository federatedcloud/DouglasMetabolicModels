model_1 = load('iNA855.mat')
model_2 = load('iNA854_argKO.mat')
model_3 = load('iNA854_hisKO.mat')

model_1=rmfield(model_1.model,'metPubChemID');
model_2=rmfield(model_2.model,'metPubChemID');
model_3=rmfield(model_3.model,'metPubChemID');

 
multi_model =  createMultipleSpeciesModel({model_2; model_3}, {'Org1'; 'Org2'}, model_1,'host')
[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'Org1'; 'Org2';'host'});

disp( multi_model.infoCom)
disp( multi_model.indCom)

rxnBiomass = strcat({'Org1'; 'Org2';'host'}, 'LP_GROW');  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;

SteadyCom(multi_model)

%%

multi_model_2 =  createMultipleSpeciesModel({model_2; model_3}, {'Org1'; 'Org2'})
[multi_model_2.infoCom, multi_model_2.indCom] = getMultiSpeciesModelId(multi_model_2, {'Org1'; 'Org2'});

disp( multi_model_2.infoCom)
disp( multi_model_2.indCom)

rxnBiomass = strcat({'Org1'; 'Org2'}, 'LP_GROW');  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model_2, rxnBiomass);  % ids
multi_model_2.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model_2.indCom.spBm = rxnBiomassId;
[sol, result, LP, LPminNorm, indLP] = SteadyCom(multi_model_2)
%SteadyComFVA(multi_model_2)

%% testing varying community 
options = struct();
options.BMgdw=[1,2]
[sol2, result2, LP2, LPminNorm2, indLP2]=SteadyCom(multi_model_2,options)

options = struct();
options.BMgdw=[1,3]
[sol3, result3, LP3, LPminNorm3, indLP3]=SteadyCom(multi_model_2,options)
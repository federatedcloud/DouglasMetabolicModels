%%% started Fall 2018, attempt to run SteadyCom on updated models

model_1 = load('080118_iNA671.L.brev.mat')      % LB
model_2 = load('073018_iNA751.A.fab.dsw54.mat') % AF
model_3 = load('072618_iNA755.A.pom.mat')       % AP
model_4 = load('061518_iNA875.L.plan.mat')      % LP
model_5 = load('061518_iNA857.A.trop.mat')       % AT


model_1=rmfield(model_1.model,'metPubChemID');
model_2=rmfield(model_2.model,'metPubChemID');
model_3=rmfield(model_3.model,'metPubChemID');
model_4=rmfield(model_4.model,'metPubChemID');
model_5=rmfield(model_5.model,'metPubChemID');

multi_model =  createMultipleSpeciesModel({model_1; model_2; model_3 ;model_4; model_5;}, {'LB';'AF';'AP';'LP';'AT'})
%can also add 'host' with model, "host"

[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'LB';'AF';'AP';'LP';'AT'});

disp( multi_model.infoCom)
disp( multi_model.indCom)

rxnBiomass ={'LBbiomass_lbrev';'AFbiomass_afab';'APbiomass_apom';'LPbiomass_lplan';'ATbiomass_atrop'};  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;

disp( multi_model.infoCom.spBm)
disp( multi_model.indCom.spBm)

SteadyCom(multi_model)
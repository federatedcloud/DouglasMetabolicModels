% TODO: this script needs to be turned into (at least one) pure function!

%
% In this scxript we verify that LB and AF can co-exist
% and be numerically simulated with vastly different population
% (i.e. biomass levels) of approx a million to 1 (below, 999999:1).

modelLB = load('080118_iNA671.L.brev.mat');      % LB
modelAF = load('073018_iNA751.A.fab.dsw54.mat'); % AF

modelLB=rmfield(modelLB.model,'metPubChemID');
modelAF=rmfield(modelAF.model,'metPubChemID');


multi_model =  createMultipleSpeciesModel({modelLB; modelAF}, {'LB';'AF'});
%can also add 'host' with model, "host"

[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'LB';'AF'});

disp( multi_model.infoCom)
disp( multi_model.indCom)

rxnBiomass ={'LBbiomass_lbrev';'AFbiomass_afab'};  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;

%disp( multi_model.infoCom.spBm)
%disp( multi_model.indCom.spBm)

options=struct();
options.algorithm = 3;

options.BMrhs = [0];
options.BMcsense = 'E';

options.BMcon = [2 -1]  % VARY biomass constraint here
[sol_1_2 result_1_2] = SteadyCom(multi_model, options)
%[fvaComMin,fvaComMax]=SteadyComFVA(multi_model)

options.BMcon=[1 -99]  % VARY biomass constraint here
[sol_99_1 result_99_1] = SteadyCom(multi_model, options)

options.BMcon=[1 -999]  % VARY biomass constraint here
[sol_999_1 result_999_1] = SteadyCom(multi_model, options)

options.BMcon=[999 -1]  % VARY biomass constraint here
[sol1_1_999 result_1_999] = SteadyCom(multi_model, options)

options.BMcon=[1 -999999]  % VARY biomass constraint here
[sol_999999_1 result_999999_1] = SteadyCom(multi_model, options)

options.BMcon=[999999 -1]  % VARY biomass constraint here
[sol1_1_999999 result_1_999999] = SteadyCom(multi_model, options)

errTol = 1e-7;
assert(abs(result_999999_1.BM(1)/result_999999_1.BM(2) - 999999) < errTol);
assert(abs(result_1_999999.BM(2)/result_1_999999.BM(1) - 999999) < errTol);


[fvaComMin,fvaComMax] = SteadyComFVA(multi_model)

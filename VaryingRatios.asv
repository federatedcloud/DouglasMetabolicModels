LB = load('022118_iNA660.L.brev.mat')
AP = load('022118_iNA755.A.pom.mat')
LPm = load('022118_iNA875.L.plan.mat')
test=load('L.brev.mat')
fba_test=optimizeCbModel(test.model)

%LBc = changeRxnBounds(LB.model,'EX_h2o(e)', 5, 'b');
%APc = changeRxnBounds(AP.model,'EX_h2o(e)', 5, 'b');
%LPc = changeRxnBounds(LPm.model,'EX_h2o(e)', 5, 'b');

%LBc = changeRxnBounds(LB.model,'EX_orn(e)', 5, 'b');
%APc = changeRxnBounds(AP.model,'EX_orn(e)', 5, 'b');
%LPc = changeRxnBounds(LPm.model,'EX_orn(e)', 5, 'b');

LBc = changeRxnBounds(LB.model,'EX_aacoa(e)', 10, 'b');
APc = changeRxnBounds(AP.model,'EX_aacoa(e)', 10, 'b');
LPc = changeRxnBounds(LPm.model,'EX_aacoa(e)', 10, 'b');

fba_LB=optimizeCbModel(LBc)
fba_AP=optimizeCbModel(APc)
fba_LP=optimizeCbModel(LPc)

LB=rmfield(LB.model,'metPubChemID');
AP=rmfield(AP.model,'metPubChemID');
LPm=rmfield(LPm.model,'metPubChemID');

multi_model =  createMultipleSpeciesModel({LB; AP; LPm}, {'LB'; 'AP';'LP'}); % creating multi-model
[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'LB'; 'AP';'LP'})

% CHANGE BOUND HERE
%multi_model= changeRxnBounds(multi_model,'EX_aacoa[u]', 5, 'b'); % constraining export reaction


% set-up functions
rxnBiomass = strcat({'LB'; 'AP';'LP'}, {'LB_GROW';'AP_GROW';'LP_GROW'});  % biomass reaction names
rxnBiomassId = findRxnIDs(multi_model, rxnBiomass);  % ids
multi_model.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
multi_model.indCom.spBm = rxnBiomassId;

% running steadyCom
[sol, result, LP, LPminNorm, indLP] = SteadyCom(multi_model)


%% testing varying community bmgdw 
options = struct();
options.BMgdw=[1,2,1]

[sol1, result1, LP1, LPminNorm1, indLP1]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,3,1]
[sol2, result2, LP2, LPminNorm2, indLP2]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,4.5,1]
[sol3, result3, LP3, LPminNorm3, indLP3]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,5,1]
[sol4, result4, LP4, LPminNorm4, indLP4]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,6,1]
[sol5, result5, LP5, LPminNorm5, indLP5]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,7,1]
[sol6, result6, LP6, LPminNorm6, indLP6]=SteadyCom(multi_model,options)

options = struct();
options.BMgdw=[1,8,1]
[sol7, result7, LP7, LPminNorm7, indLP7]=SteadyCom(multi_model,options)

%% varying GRfx
options=struct();
options.GRfx=[1,0.15]
[sol8, result8, LP8, LPminNorm8, indLP8]=SteadyCom(multi_model,options)
%%% varying 


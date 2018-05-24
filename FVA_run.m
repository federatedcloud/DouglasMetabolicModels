LB = load('040318_L.brev.mat')
AP = load('040318_A.pom.mat')
LPm = load('040318_L.plan.mat')
AT = load('040318_A.trop.mat')
AF = load('040318_A.dsw54.mat')

LB=rmfield(LB.model,'metPubChemID');
AP=rmfield(AP.model,'metPubChemID');
LPm=rmfield(LPm.model,'metPubChemID');
AT=rmfield(AT.model,'metPubChemID');
AF=rmfield(AF.model,'metPubChemID');

[LB_FVA_min, LB_FVA_max]=fluxVariability(LB)
[AP_FVA_min, AP_FVA_max]=fluxVariability(AP)
[LPm_FVA_min, LPm_FVA_max]=fluxVariability(LPm)
[AT_FVA_min, AT_FVA_max]=fluxVariability(AT)
[AF_FVA_min, AF_FVA_max]=fluxVariability(AF)

LB_model =  createMultipleSpeciesModel({LB}, {'LB'}); % creating multi-model
AP_model =  createMultipleSpeciesModel({AP}, {'AP'});
LPm_model =  createMultipleSpeciesModel({LPm}, {'LPm'});
AT_model =  createMultipleSpeciesModel({AT}, {'AT'});
AF_model =  createMultipleSpeciesModel({AF}, {'AF'});

[LB_model.infoCom, LB_model.indCom] = getMultiSpeciesModelId(LB_model, {'LB'})
[AP_model.infoCom, AP_model.indCom] = getMultiSpeciesModelId(AP_model, {'AP'})
[LPm_model.infoCom, LPm_model.indCom] = getMultiSpeciesModelId(LPm_model, {'LP'})
[AT_model.infoCom, AT_model.indCom] = getMultiSpeciesModelId(AT_model, {'AT'})
[AF_model.infoCom, AF_model.indCom] = getMultiSpeciesModelId(AF_model, {'AF'})

% set-up functions
rxnBiomass1 = strcat({'LB'}, {'LB_GROW'});  % biomass reaction names
rxnBiomass2 = strcat({'AP'}, {'AP_GROW'});
rxnBiomass3 = strcat({'LPm'}, {'LP_GROW'});
rxnBiomass4 = strcat({'AT'}, {'AT_GROW'});
rxnBiomass5 = strcat({'AF'}, {'ADSW_GROW'});

rxnBiomassId1 = findRxnIDs(LB_model, rxnBiomass1);  % ids
rxnBiomassId2 = findRxnIDs(AP_model, rxnBiomass2);
rxnBiomassId3 = findRxnIDs(LPm_model, rxnBiomass3);
rxnBiomassId4= findRxnIDs(AT_model, rxnBiomass4);
rxnBiomassId5 = findRxnIDs(AF_model, rxnBiomass5);

LB_model.indCom.spBm = rxnBiomassId1;
AP_model.indCom.spBm = rxnBiomassId2;
LPm_model.indCom.spBm = rxnBiomassId3;
AT_model.indCom.spBm = rxnBiomassId4;
AF_model.indCom.spBm = rxnBiomassId5;


LB_model.infoCom.spBm = rxnBiomass1;  % .spBm for organism biomass reactions
AP_model.infoCom.spBm = rxnBiomass2;
LPm_model.infoCom.spBm = rxnBiomass3;
AT_model.infoCom.spBm = rxnBiomass4;
AF_model.infoCom.spBm = rxnBiomass5;


%optionsTest.BMweight=0.6;
%LB_model.csense = char('E' * ones(1,numel(LB_model.mets)));

% running steadyCom
[sol_LB, result_LB, LP_LB, LPminNorm_LB, indLP_LB] = SteadyCom(LB_model)
[sol_AP, result_AP, LP_AP, LPminNorm_AP, indLP_AP] = SteadyCom(AP_model)
[sol_LPm, result_LPm, LP_LPm, LPminNorm_LPm, indLP_LPm] = SteadyCom(LPm_model)
[sol_AT, result_AT, LP_AT, LPminNorm_AT, indLP_AT] = SteadyCom(AT_model)
[sol_AF, result_AF, LP_AF, LPminNorm_AF, indLP_AF] = SteadyCom(AF_model)



% percentage of maximum total biomass of the community required. 100 for sum(biomass) = 1 (1 is the default total biomass)
%options.optBMpercent = 100;  
%n = size(AT_model.S, 1);  % number of reactions in the model
% options.rxnNameList is the list of reactions subject to FVA. Can be reaction names or indices.
% Use n + j for the biomass variable of the j-th organism. Alternatively, use {'X_j'} 
% for biomass variable of the j-th organism or {'X_Ec1'} for Ec1 (the abbreviation in EcCom.infoCom.spAbbr)
%options.rxnNameList = {'AT'};

%options.optGRpercent = [89:0.2:99, 99.1:0.1:100];  % perform FVA at various percentages of the maximum growth rate, 89, 89.1, 89.2, ..., 100
options = struct();
options.rxnNameList = LB_model.rxns;
options.GRpercent=100.00
%options.GRmax=100
[minFlux_LB, maxFlux_LB,minFD_LB, maxFD_LB, GRvector_LB, result_LB, LP_LB] = SteadyComFVA(LB_model, options)

options.rxnNameList = AP_model.rxns;
[minFlux_AP, maxFlux_AP,minFD_AP, maxFD_AP, GRvector_AP, result_AP, LP_AP] = SteadyComFVA(AP_model, options)

options.rxnNameList = LPm_model.rxns;
[minFlux_LPm, maxFlux_LPm,minFD_LPm, maxFD_LPm, GRvector_LPm, result_LPm, LP_LPm] = SteadyComFVA(LPm_model, options)

options.rxnNameList = AT_model.rxns;
[minFlux_AT, maxFlux_AT,minFD_AT, maxFD_AT, GRvector_AT, result_AT, LP_AT] = SteadyComFVA(AT_model, options)

options.rxnNameList = AF_model.rxns;
[minFlux_AF, maxFlux_AF,minFD_AF, maxFD_AF, GRvector_AF, result_AF, LP_AF] = SteadyComFVA(AF_model, options)

%% multimodel FVA
[mult_LB_FVA_min, mult_LB_FVA_max]=fluxVariability(LB_model)
[mult_AP_FVA_min, mult_AP_FVA_max]=fluxVariability(AP_model)
[mult_LPm_FVA_min, mult_LPm_FVA_max]=fluxVariability(LPm_model)
[mult_AT_FVA_min, mult_AT_FVA_max]=fluxVariability(AT_model)
[mult_AF_FVA_min, mult_AF_FVA_max]=fluxVariability(AF_model)

%%
options = struct();
options.rxnNameList = LB_model.rxns;
options.GRpercent=100.00
options.GRmax=100
[minFlux_LB, maxFlux_LB,minFD_LB, maxFD_LB, GRvector_LB, result_LB, LP_LB] = SteadyComFVA(LB_model, options)

options.rxnNameList = AP_model.rxns;
[minFlux_AP, maxFlux_AP,minFD_AP, maxFD_AP, GRvector_AP, result_AP, LP_AP] = SteadyComFVA(AP_model, options)

options.rxnNameList = LPm_model.rxns;
[minFlux_LPm, maxFlux_LPm,minFD_LPm, maxFD_LPm, GRvector_LPm, result_LPm, LP_LPm] = SteadyComFVA(LPm_model, options)

options.rxnNameList = AT_model.rxns;
[minFlux_AT, maxFlux_AT,minFD_AT, maxFD_AT, GRvector_AT, result_AT, LP_AT] = SteadyComFVA(AT_model, options)

options.rxnNameList = AF_model.rxns;
[minFlux_AF, maxFlux_AF,minFD_AF, maxFD_AF, GRvector_AF, result_AF, LP_AF] = SteadyComFVA(AF_model, options)






%%
grComV = result.GRmax * options.optGRpercent / 100;  % vector of growth rates tested
lgLabel = {'{\itEc1 }';'{\itEc2 }';'{\itEc3 }';'{\itEc4 }'};
col = [235 135 255; 0 235 0; 255 0 0; 95 135 255 ]/255;  % color
f = figure;
% SteadyCom
subplot(2, 1, 1);
hold on
x = [grComV(:); flipud(grComV(:))];
for j = 1:4
    y = [fvaComMin(j, :), fliplr(fvaComMax(j, :))];
    p(j, 1) = plot(x(~isnan(y)), y(~isnan(y)), 'LineWidth', 2);
    p(j, 1).Color = col(j, :);
end
tl(1) = title('\underline{SteadyCom}', 'Interpreter', 'latex');
tl(1).Position = [0.7 1.01 0];
ax(1) = gca;
ax(1).XTick = 0.66:0.02:0.74;
ax(1).YTick = 0:0.2:1;
xlim([0.66 0.74])
ylim([0 1])

lg = legend(lgLabel);
lg.Box = 'off';
yl(1) = ylabel('Relative abundance');
xl(1) = xlabel('Community growth rate (h^{-1})');
% FBA
grFBAV = grFBA * optGRpercentFBA / 100;
x = [grFBAV(:); flipud(grFBAV(:))];
subplot(2, 1, 2);
hold on
% plot j=1:2 only because 3:4 overlap with 1:2
for j = 1:2
    y = [fvaFBAMin(j, :), fliplr(fvaFBAMax(j, :))] ./ x';
    % it is possible some values > 1 because the total biomass produced is
    % only bounded below when calling fluxVariability. Would be strictly
    % equal to 1 if sum(biomass) = optGRpercentFBA(jGr) * grFBA is constrained. Treat them as 1.
    y(y>1) = 1;
    p(j, 2)= plot(x(~isnan(y)), y(~isnan(y)), 'LineWidth', 2);
    p(j, 2).Color = col(j, :);
end
tl(2) = title('\underline{Joint FBA}', 'Interpreter', 'latex');
tl(2).Position = [0.55 1.01 0];
ax(2) = gca;
ax(2).XTick = 0.52:0.02:0.58;
ax(2).YTick = 0:0.2:1;
xlim([0.52 0.58])
ylim([0 1])
xl(2) = xlabel('Community growth rate (h^{-1})');
yl(2) = ylabel('Relative abundance');
ax(1).Position = [0.1 0.6 0.5 0.32];
ax(2).Position = [0.1 0.1 0.5 0.32];
lg.Position = [0.65 0.65 0.1 0.27];


figure
hold on
subplot(2,2,1)
scatter(mult_AP_FVA_max, maxFlux_AP)
hold on
scatter(mult_AP_FVA_min, minFlux_AP)
[max_AP_rho,AP_pval] = corr(mult_AP_FVA_max, maxFlux_AP,'Type','Spearman')
[min_AP_rho,AP_pval] = corr(mult_AP_FVA_min, minFlux_AP,'Type','Spearman')
title('Acetobacter pomorum: SteadyComFVA vs FVA')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")


%{
figure
hold on
scatter(AP_FVA_max, maxFlux_AP(1:1058))
scatter(AP_FVA_min, minFlux_AP(1:1058))
title('Pomorum: SteadyComFVA vs FVA(no exchange)')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")
hold off
%}




subplot(2,2,2)
scatter(mult_AT_FVA_max, maxFlux_AT)
hold on
scatter(mult_AT_FVA_min, minFlux_AT)
[max_AT_rho,AT_pval] = corr(mult_AT_FVA_max, maxFlux_AT,'Type','Spearman')
[min_AT_rho,AT_pval] = corr(mult_AT_FVA_min, minFlux_AT,'Type','Spearman')
title('Actobacter tropicalis: SteadyComFVA vs FVA')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")


%{
scatter(AT_FVA_max, maxFlux_AT(1:1228))
scatter(AT_FVA_min, minFlux_AT(1:1228))
title('Tropicalis: SteadyComFVA vs FVA(no exchange)')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")
hold off
%}


ax=subplot(2,2,3) 
scatter(ax,mult_LB_FVA_max, maxFlux_LB)
hold on
scatter(ax,mult_LB_FVA_min, minFlux_LB)
[max_LB_rho,LB_pval] = corr(mult_LB_FVA_max, maxFlux_LB,'Type','Spearman')
[min_LB_rho,LB_pval] = corr(mult_LB_FVA_min, minFlux_LB,'Type','Spearman')
title('Lactobacillus brevis: SteadyComFVA vs FVA')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")


%{
figure
hold on
scatter(LB_FVA_max, maxFlux_LB(1:946))
scatter(LB_FVA_min, minFlux_LB(1:946))
title('Brevis: SteadyComFVA vs FVA(no exchange)')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")
hold off
%}

subplot(2,2,4) 
scatter(mult_LPm_FVA_max, maxFlux_LPm)
hold on
scatter(mult_LPm_FVA_min, minFlux_LPm)
[max_LPm_rho,LPm_pval] = corr(mult_LPm_FVA_max, maxFlux_LPm,'Type','Spearman')
[min_LPm_rho,LPm_pval] = corr(mult_LPm_FVA_min, minFlux_LPm,'Type','Spearman')
title('Lactobacillus plantarum: SteadyComFVA vs FVA')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")

hold off 

%{
figure
hold on
scatter(LPm_FVA_max, maxFlux_LPm(1:1028))
scatter(LPm_FVA_min, minFlux_LPm(1:1028))
title('Plantarum: SteadyComFVA vs FVA(no exchange)')
legend('max','min')
xlabel("FVA")
ylabel("SteadyComFVA")
hold off
%}


%%

% FVA vs FVA 

figure
hold on
scatter(AP_FVA_max, mult_AP_FVA_max(1:1058))
scatter(AP_FVA_min, mult_AP_FVA_min(1:1058))
title('Acetobacter pomorum: FVA multi vs FVA single')
legend('max','min')
xlabel("FVA single")
ylabel("FVA multi")
hold off


figure
hold on
scatter(AT_FVA_max, mult_AT_FVA_max(1:1228))
scatter(AT_FVA_min, mult_AT_FVA_min(1:1228))
title('Actobacter tropicalis: FVA multi vs FVA single')
legend('max','min')
xlabel("FVA single")
ylabel("FVA multi")
hold off


figure
hold on
scatter(LB_FVA_max, mult_LB_FVA_max(1:946))
scatter(LB_FVA_min, mult_LB_FVA_min(1:946))
title('Lactobacillus brevis: FVA multi vs FVA single')
legend('max','min')
xlabel("FVA single")
ylabel("FVA multi")
hold off



figure
hold on
scatter(LPm_FVA_max, mult_LPm_FVA_max(1:1028))
scatter(LPm_FVA_min, mult_LPm_FVA_min(1:1028))
title('Lactobacillus plantarum: FVA multi vs FVA single')
legend('max','min')
xlabel("FVA single")
ylabel("FVA multi")
hold off




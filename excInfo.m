%% extraction of exchange reaction data
% Authors: Cindy Wu
function [minIdx, richIdx] = excInfo(model, nameTag) %model_FVA_min, model_FVA_max, modelR_FVA_min, modelR_FVA_max)
% produce bar graph and pie chart with relevant exchange rxn information and
% returns cell arrays corresponding to the pie chart data
% minIdx: n x 1 cell array where n is the number of sections in the
% pie chart and each cell contains a list of rxnIDs corresponding to that
% section for the microbe in a minimum environment
% richIdx: n x 1 cell array, returns the same thing as minIdx but for a
% microbe in a rich environment

% analyzes all exchange reactions and groups the reaction by comparing the 
% FVA and FBA run of the model 
% CALCULATION: (FVA_max-FVA_min)/FBAsoln 

% note: if FVA data is already stored, uncomment FVA arguments and comment
% FVA runs in code to make code run faster.

excIDs=findExcIDs(model);

% minimum model flux analysis
minFBAsoln = optimizeCbModel(model);
[model_FVA_min, model_FVA_max]=fluxVariability(model); % FVA run
[minFBA_countY]=countFlux(excIDs, minFBAsoln.x);
[minFVA_countY, minFVA_diff]=countFlux(excIDs, model_FVA_min, model_FVA_max); 

% rich model flux analysis
model_rich = createRich(model);
richFBAsoln = optimizeCbModel(model_rich);
[modelR_FVA_min, modelR_FVA_max]=fluxVariability(model_rich); % FVA run
[richFBA_countY]=countFlux(excIDs, richFBAsoln.x);
[richFVA_countY, richFVA_diff]=countFlux(excIDs, modelR_FVA_min, modelR_FVA_max);

% flux count - bar graph
data = [minFBA_countY; richFBA_countY; minFVA_countY; richFVA_countY];
barGraph=figure();
b = bar(data, 'FaceColor', 'flat');
title(['Flux Counts for FVA and FBA Analysis for: ', nameTag])
b.CData(2,:)= [0.8 0 0];
b.CData(4,:)= [0.8 0 0];
name = {'min FBA'; 'rich FBA'; 'min FVA'; 'rich FVA'};
set(gca,'xticklabel',name)

% flux variability analysis
min_compare = FVAvsFBA(excIDs, minFBAsoln, minFVA_diff);
rich_compare = FVAvsFBA(excIDs, richFBAsoln, richFVA_diff);

% flux variability analysis - pie chart
% minimum model data
labels = {'0%', '0-1%', '1-100%', '100-1000%', '>1000%'};
[minData, minIdx] = compareCount(excIDs, min_compare);
figure()
pie(minData)
legend(labels, 'Location', 'northeastoutside')
title(['minimum model:  ', nameTag])

% rich model data
[richData, richIdx] = compareCount(excIDs, rich_compare);
figure()
pie(richData)
legend(labels, 'Location', 'northeastoutside')
title(['rich model:  ', nameTag])

end

%% subfunctions
function [pieChartData, idx] = compareCount(excIDs, comparison)
% Extracts pie chart data array from FVA variability vs FBA data
% pieChartData: the flux count for each slice of pie chart (0%, 0-1%,
% 1-100%, >1000%)
% idx: 2D array representing the indices of the reaction corresponding to
% each slice of the pie chart
pieChartData = zeros(1,5);
idx=cell(1,5);
for k=excIDs
    if comparison(k)==0
        idx{1}(length(idx{1})+1)=k;
        pieChartData(1) = pieChartData(1)+1;
    elseif comparison(k)>0 && comparison(k)<1
        idx{2}(length(idx{2})+1)= k;
        pieChartData(2) = pieChartData(2)+1;
    elseif comparison(k)>=1 && comparison(k)<=100
        idx{3}(length(idx{3})+1)= k;
        pieChartData(3) = pieChartData(3)+1;
    elseif comparison(k)>100 && comparison(k)<=1000
        idx{4}(length(idx{4})+1)= k;
        pieChartData(4) = pieChartData(4)+1;
    else
        idx{5}(length(idx{5})+1)= k;
        pieChartData(5) = pieChartData(5)+1;
    end
end
end

function comparison = FVAvsFBA(excIDs, FBA_run, FVA_diff)
% Compares FVA variability and FBA data for each reaction in excIDs
% comparison(k) = (FVA_max(k)-FVA_min(k))/(FBA(k))

for k = excIDs
    if FBA_run.x(k)==0
        comparison(k)=0;
    else
        comparison(k)=FVA_diff(k)/abs(FBA_run.x(k))*100;
    end
end
comparison=transpose(comparison);
end

function [countY, fluxdiff] = countFlux(excIDs, fluxArray_min, fluxArray_max)
% Counts number of exchange reactions that carry flux given FVA and FBA
% data. Given FBA data array, it should be entered as fluxArray_min.
% A reaction is considered to carry flux if the flux is >10^-6
% For FVA, a reaction is considered to carry flux if both the min and max
% fluxes are >10^-6. 
% INPUTS------------------------------------------------------------------
% excIDs: row vector of reaction IDs to be analyzed
% fluxArray_min: either the FBA solution corresponding to the reactions in
% excIDs, or the FVA minimum solution
% fluxArray_max: (OPTIONAL for FBA) FVA maximum solution
% OUTPUTS-----------------------------------------------------------------
% countY: number of reactions that carry flux
% fluxdiff: array containing the differences between FVA_max and FVA_min for all
% reactions

countY=0;
countN=0;
len=length(fluxArray_min);
% if the flux count is for FBA
if nargin ~= 3
    fluxArray_max=zeros(len);
end

for k=excIDs %change from excIDs for non-exchange rxns
    if abs(fluxArray_min(k))<10^-6 %flux is significant only if >10^-6
        fluxArray_min(k)=0;
    end
    if abs(fluxArray_max(k))<10^-6
        fluxArray_max(k)=0;
    end
        fluxdiff(k)=fluxArray_max(k)-fluxArray_min(k);
    if fluxdiff(k)~=0
        countY=countY+1;
    end
end
fluxdiff=transpose(fluxdiff);
end
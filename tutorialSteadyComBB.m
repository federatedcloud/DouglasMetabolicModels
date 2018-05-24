%% Analyze Steady-State Community COBRA Models at  Using SteadyCom
%% Author(s): Siu Hung Joshua Chan, Department of Chemical Engineering, The Pennsylvania State University
%% Reviewer(s): Almut Heinken, Luxembourg Centre for Systems Biomedicine, University of Luxembourg
% __
%% INTRODUCTION
% This tutorial demonstrates the use of SteadyCom to analyze a multi-organism 
% COBRA model (e.g., for a microbial community) at a community steady-state [1]. 
% Compared to the direct extension of flux balance analysis (FBA) which simply 
% treats a community model as a multi-compartment model, SteadyCom explicitly 
% introduces the biomass variables to describe the relationships between biomass, 
% biomass production rate, growth rate and fluxes. SteadyCom also assumes the 
% existence of a time-averaged population steady-state for a stable microbial 
% community which in turn implies a time-averaged constant growth rate across 
% all members. SteadyCom is equivalent to the reformulation of the earlier community 
% flux balance analysis (cFBA) [2] with significant computational advantage. SteadyCom 
% computes the maximum community growth rate by solving the follow optimization 
% problem:
% 
% $$\begin{array}{ll}\max & \ \mu\\ \\\text{s.t.} & \sum\limits_{j\in \textbf{J}^k}S^k_{ij}V^k_j=0, 
% & \forall i \in \textbf{I}^k, k \in \textbf{K}\\ & LB^k_jX^k\leq V^k_j\leq UB^k_jX^k, 
% & \forall j \in \textbf{J}^k, k \in \textbf{K} \\& \sum\limits_{k \in \textbf{K}}V^k_{ex(i)} 
% + u^{com}_i\geq 0, & \forall i \in \textbf{I}^{com} \\& V^k_{biomass} = X^k\mu, 
% & \forall k \in \textbf{K} \\& \sum\limits_{k \in \textbf{K}}X^k = 1 \\& X^k,\quad 
% \mu \geq 0, & \forall k \in \textbf{K} \\& V^k_j \in \Re, & \forall j \in \textbf{J}^k, 
% k \in \textbf{K} \end{array}$$
% 
% where $S^k_{ij}$ is the stoichiometry of metabolite _i_ in reaction _j_ 
% for organism _k_, $V^k_j$, $LB^k_j$ and $UB^k_j$ are respectively the flux (in 
% mmol/h), lower bound (in mmol/h/gdw) and upper bound (in mmol/h/gdw) for reaction 
% _j_ for organism _k_, $u^{com}_i$ is the community uptake bound for metabolite 
% _i_, $X^k$ is the biomass (in gdw) of organism _k_, $\mu$ is the community growth 
% rate, $\textbf{I}^k$ is the set of metabolites of organism _k_, $\textbf{I}^{com}$ 
% is the set of community metabolites in the community exchange space, $\textbf{J}^k$ 
% is the set of reactions for organism k, $\textbf{K}$ is the set of organisms 
% in the community, and $ex(i) \in \textbf{J}^k$ is the exchange reaction in organism 
% _k_ for extracellular metabolite _i_. See ref. [1] for the derivation and detailed 
% explanation.
% 
% Throughout the tutorial, using a hypothetical model of four _E. coli_ mutants 
% auxotrophic for amino acids, we will demonstrate the three different functionalities 
% of the module: (1) computing the maximum community growth rate using the function 
% SteadyCom.m, (2) performing flux variability analysis under a given community 
% growth rate using SteadyComFVA.m, and (3) analyzing the pairwise relationship 
% between flux/biomass variables using a technique similar to Pareto-optimal analysis 
% by calling the function SteadyComPOA.m
%% EQUIPMENT SETUP
% If necessary, initialise the cobra toolbox and select a solver by running:

% initCobraToolbox
%% 
% All SteadyCom functions involve only solving linear programming problems. 
% Any solvers supported by the COBRA toolbox will work. But SteadyCom contains 
% specialized codes for IBM ILOG Cplex which was tested to run significantly faster 
% for SteadyComFVA and SteadyComPOA for larger problems through calling the Cplex 
% object in Matlab directly. For a guide how to install solvers, please refer 
% to the <https://github.com/opencobra/cobratoolbox/blob/master/docs/source/installation/solvers.md 
% opencobra documentation>.
% 
% Please note that parallelization requires a working installation of the 
% Parallel Computing Toolbox.

changeCobraSolver('ibm_cplex', 'LP');
%% PROCEDURE
%% Model Construction
% Load the _E. coli_ iAF1260 model in the COBRA toolbox.

global CBTDIR
iAF1260 = readCbModel([CBTDIR filesep 'test' filesep 'models' filesep 'mat' filesep 'iAF1260.mat']);
%% 
% Polish the model a little bit:

% convert the compartment format from e.g., '_c' to '[c]'
iAF1260.mets = regexprep(iAF1260.mets, '_([^_]+)$', '\[$1\]');
% make all empty cells in cell arrays to be empty string
fieldToBeCellStr = {'metFormulas'; 'genes'; 'grRules'; 'metNames'; 'rxnNames'; 'subSystems'};
for j = 1:numel(fieldToBeCellStr)
    iAF1260.(fieldToBeCellStr{j})(cellfun(@isempty, iAF1260.(fieldToBeCellStr{j}))) = {''};
end
%% 
% Add a methionine export reaction to allow the export of methionine.

iAF1260 = addReaction(iAF1260,{'METt3pp',''},'met__L[c] + h[c] => met__L[p] + h[p]');
%% 
% Reactions essential for amino acid autotrophy:

argH = {'ARGSL'};  % essential for arginine biosynthesis
lysA = {'DAPDC'};  % essential for lysine biosynthesis
metA = {'HSST'};  % essential for methionine biosynthesis
ilvE = {'PPNDH'};  % essential for phenylalanine biosynthesis
%% 
% Reactions essential for exporting amino acids:

argO = {'ARGt3pp'};  % Evidence for an arginine exporter encoded by yggA (argO) that is regulated by the LysR-type transcriptional regulator ArgP in Escherichia coli.
lysO = {'LYSt3pp'};  % Distinct paths for basic amino acid export in Escherichia coli: YbjE (LysO) mediates export of L-lysine
yjeH = {'METt3pp'};  % YjeH is a novel L-methionine and branched chain amino acids exporter in Escherichia coli
yddG = {'PHEt2rpp'};  % YddG from Escherichia coli promotes export of aromatic amino acids.
%% 
% Now make four copies of the model with auxotrophy for different amino 
% acids and inability to export amino acids:

% auxotrophic for Lys and Met, not exporting Phe
Ec1 = iAF1260;
Ec1 = changeRxnBounds(Ec1, [lysA; metA; yddG], 0, 'b');
% auxotrophic for Arg and Phe, not exporting Met
Ec2 = iAF1260;
Ec2 = changeRxnBounds(Ec2, [argH; yjeH; ilvE], 0, 'b');
% Auxotrophic for Arg and Phe, not exporting Lys
Ec3 = iAF1260;
Ec3 = changeRxnBounds(Ec3, [argH; lysO; ilvE], 0, 'b');
% Auxotrophic for Lys and Met, not exporting Arg
Ec4 = iAF1260;
Ec4 = changeRxnBounds(Ec4, [argO; lysA; metA], 0, 'b');
%% 
% Now none of the four organisms can grow alone and they must cross feed 
% each other to survive. See Figure 1 in ref. <http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005539 
% [1]> for the visualization of the community. 
% 
% Get the extracellular metabolites, the corresponding exchange reactions 
% and the uptake rates for the _E. coli_ model, which are used later to constrain 
% the community model:

% extracellular metabolites (met[e])
metEx = strcmp(getCompartment(iAF1260.mets),'e');
% the corresponding exchange reactions
rxnExAll = find(sum(iAF1260.S ~= 0, 1) == 1);
[rxnEx, ~] = find(iAF1260.S(metEx, rxnExAll)');  % need to be in the same order as metEx
rxnEx = rxnExAll(rxnEx);
% exchange rate
lbEx = iAF1260.lb(rxnEx);
%% 
% Create a community model with the four _E. coli_ tagged as 'Ec1', 'Ec2', 
% 'Ec3', 'Ec4' respectively by calling |createMultipleSpeciesModel|. 

nameTagsModel = {'Ec1'; 'Ec2'; 'Ec3'; 'Ec4'};
EcCom = createMultipleSpeciesModel({Ec1; Ec2; Ec3; Ec4}, nameTagsModel);
EcCom.csense = char('E' * ones(1,numel(EcCom.mets)));  % correct the csense
clear Ec1 Ec2 Ec3 Ec4
%% 
% The model |EcCom| contains a community compartment denoted by |[u]| to 
% allow exchange between organisms. Each organism-specific reaction/metabolite 
% is prepended with the corresponding tag.
% 
% Retreive the names and ids for organism/community exchange reactions/metabolites 
% which are necessary for computation:

[EcCom.infoCom, EcCom.indCom] = getMultiSpeciesModelId(EcCom, nameTagsModel);
disp(EcCom.infoCom);
%% 
% |EcCom.infoCom |contains reaction/metabolite names (from |EcCom.rxns|/|EcCom.mets|) 
% for the community exchange reactions (|*.EXcom|), organism-community exchange 
% reactions (|*.EXsp|), community metabolites (|*.Mcom|), organism-specific extracellular 
% metabolite (|*.Msp|). If a host model is specified, there will also be non-empty 
% |*.EXhost| and |*.Mhost |for the host-specific exchange reactions and metabolites. 
% The fields |*.rxnSps|/|*.metSps| give information on which organism a reaction/metabolite 
% belongs to.
% 
% |indCom |has the same structure as |infoCom| but contains the indices rather 
% than names. |infoCom| and |indCom| are attached as fields of the model |EcCom| 
% because SteadyCom requires this information from the input model for computation. 
% Incorporate also the names and indices for the biomass reactions which are necessary 
% for computing growth:

rxnBiomass = strcat(nameTagsModel, 'BIOMASS_Ec_iAF1260_core_59p81M');  % biomass reaction names
rxnBiomassId = findRxnIDs(EcCom, rxnBiomass);  % ids
EcCom.infoCom.spBm = rxnBiomass;  % .spBm for organism biomass reactions
EcCom.indCom.spBm = rxnBiomassId;
%% 
% 
%% Finding Maximum Growth Rate Using SteadyCom
% Set community and organism-specific uptake rates to be the same as in the 
% orginal iAF1260 model:

[yn, id] = ismember(strrep(iAF1260.mets(metEx), '[e]', '[u]'), EcCom.infoCom.Mcom);  % map the metabolite name
assert(all(yn));  % must be a 1-to-1 mapping
EcCom.lb(EcCom.indCom.EXcom(:,1)) = lbEx(id);  % assign community uptake bounds
EcCom.ub(EcCom.indCom.EXcom(:,1)) = 1e5;
EcCom.lb(EcCom.indCom.EXsp) = repmat(lbEx(id), 1, 4);  % assign organism-specific uptake bounds
%% 
% Set maximum allowed organism-specific uptake rates for the cross-feeding 
% amino acids:

% only allow to take up the amino acids that one is auxotrophic for
exRate = 1;  % maximum uptake rate for cross feeding AAs
% Ec1
EcCom = changeRxnBounds(EcCom, {'Ec1IEX_arg__L[u]tr'; 'Ec1IEX_phe__L[u]tr'}, 0, 'l');
EcCom = changeRxnBounds(EcCom, {'Ec1IEX_met__L[u]tr'; 'Ec1IEX_lys__L[u]tr'}, -exRate, 'l');
% Ec2
EcCom = changeRxnBounds(EcCom, {'Ec2IEX_arg__L[u]tr'; 'Ec2IEX_phe__L[u]tr'}, -exRate, 'l');
EcCom = changeRxnBounds(EcCom, {'Ec2IEX_met__L[u]tr'; 'Ec2IEX_lys__L[u]tr'}, 0, 'l');
% Ec3
EcCom = changeRxnBounds(EcCom, {'Ec3IEX_arg__L[u]tr'; 'Ec3IEX_phe__L[u]tr'}, -exRate, 'l');
EcCom = changeRxnBounds(EcCom, {'Ec3IEX_met__L[u]tr'; 'Ec3IEX_lys__L[u]tr'}, 0, 'l');
% Ec4
EcCom = changeRxnBounds(EcCom, {'Ec4IEX_arg__L[u]tr'; 'Ec4IEX_phe__L[u]tr'}, 0, 'l');
EcCom = changeRxnBounds(EcCom, {'Ec4IEX_met__L[u]tr'; 'Ec4IEX_lys__L[u]tr'}, -exRate, 'l');
% allow production of anything for each member
EcCom.ub(EcCom.indCom.EXsp(:)) = 1000;
%% 
% Before the calculation, print the community uptake bounds for checking 
% using |printUptakeBoundCom|:

printUptakeBoundCom(EcCom, 1);
%% 
% Values under 'Comm.' are the community uptake bounds (+ve for uptake) 
% and values under 'Ec1' are the Ec1-specific uptake bounds (-ve for uptake). 
% 
% Create an option structure for calling SteadyCom and call the function. 
% There are a range of options available, including setting algorithmic parameters, 
% fixing growth rates for members, adding additional linear constraints in a general 
% format, e.g., for molecular crowding effect. See |help SteadyCom |for more options.

options = struct();
options.GRguess = 0.5;  % initial guess for max. growth rate
options.GRtol = 1e-6;  % tolerance for final growth rate
options.algorithm = 1;  % use the default algorithm (simple guessing for bounds, followed by matlab fzero)
[sol, result] = SteadyCom(EcCom, options);
%% 
% The algorithm is an iterative procedure to find the maximum biomass at 
% a given growth rate and to determine the maximum growth rate that is feasible 
% for the required total biomass (default 1 gdw). Here the algorithm used is the 
% simple guessing for find upper and lower bounds (Iter 1 to 4 in the output) 
% followed by Matlab |fzero| (starting from the line '|Func-count|') to locate 
% the root. The maximum growth rate calculated is 0.73599 /h, stored in |result.GRmax|._ 
% _
% 
% The biomass for each organism (in gdw) is given by_ |_result.BM|:

for jSp = 1:4
    fprintf('X_%s:  %.6f\n', EcCom.infoCom.spAbbr{jSp}, result.BM(jSp));
end
disp(result);
%% 
% |result.vBM| contains the biomass production rates (in gdw / h), equal 
% to |result.BM * result.GRmax. |Since the total community biomass is defaulted 
% to be 1 gdw, the biomass for each organism coincides with its relative abundance. 
% Note that the community uptake bounds in this sense are normalized per gdw of 
% the community biomass. So the lower bound for the exchange reaction |EX_glc__D[u]| 
% being 8 can be interpreted as the maximum amount of glucose available to the 
% community being at a rate of 8 mmol per hour for 1 gdw of community biomass. 
% Similarly, all fluxes in |result.flux |($V^k_j$)| |has the unit mmol / h / [gdw 
% of comm. biomass]. It differs from the specific rate (traditionally denoted 
% by $v^k_j$) of an organism in the usual sense (in the unit of mmol / h / [gdw 
% of organism biomass]) by $V^k_j=X^kv^k_j$ where $X^k$ is the biomass of the 
% organism. |result.Ut|_ _and_ |_result.Ex |are the community uptake and export 
% rates respectively, corresponding to the exchange reactions in |EcCom.infoCom.EXcom|.| 
% |
% 
% |result.iter0 |is the info for solving the model at zero growth rate and 
% |result.iter |records the info during iteration of the algorithm:


iter = [0, result.iter0, NaN; result.iter];
for j = 0 : size(iter, 1)
    if j == 0
        fprintf('#iter\tgrowth rate (mu)\tmax. biomass (sum(X))\tmu * sum(X)\tmax. infeasibility\tguess method\n');
    else
        fprintf('%5d\t%16.6f\t%21.6f\t%11.6f\t%18.6e\t%d\n', iter(j,:))
    end
end
%% 
% |mu * sum(X)| in the forth column is equal to the biomass production rate. 
% 
% The fifth column contains the maximum infeasibility of the solutions in 
% each iteration.
% 
% Guess method in the last column represents the method used for guessing 
% the growth rate solved in the current iteration:
% 
% 0: the default simple guess by $<math xmlns="http://www.w3.org/1998/Math/MathML" 
% display="inline"><mrow><msub><mrow><mi>&mu;</mi></mrow><mrow><mi mathvariant="normal">next</mi></mrow></msub><mo>=</mo><msub><mrow><mi>&mu;</mi></mrow><mrow><mi 
% mathvariant="normal">current</mi></mrow></msub><mtext>?</mtext><mrow><msubsup><mrow><mo>&sum;</mo></mrow><mrow><mi 
% mathvariant="italic">k</mi><mo>=</mo><mn>1</mn></mrow><mrow><mi mathvariant="italic">K</mi></mrow></msubsup><mrow><msubsup><mrow><mi 
% mathvariant="italic">X</mi></mrow><mrow><mi mathvariant="italic">k</mi></mrow><mrow><mi 
% mathvariant="normal">current</mi></mrow></msubsup></mrow></mrow></mrow></math>$ 
% (_K_ is the total number of organisms)
% 
% 1: bisection method
% 
% 2: bisection or at least 1% away from the bounds if the simple guess is 
% too close to the bounds (<1%)
% 
% 3. 1% away from the current growth rate if the simple guess is too close 
% to the current growth rate
% 
% From the table, we can see that at the growth rate 0.742726 (iter 4), the 
% max. biomass is 0, while at growth rate 0.735372, max. biomass = 1.0008 > 1. 
% Therefore we have both an lower and upper bound for the max. growth rate. Then 
% fzero is initiated to solve for the max. growth rate that gives max. biomass 
% >= 1.
% 


disp("*****************   Following are the constrained results ********************");

optionsTest = options;
optionsTest.BMcon=[1 1 1 1];
optionsTest.BMrhs=[1];
optionsTest.BMcsense=['E'];
optionsTest.BMweight=0.6;
[sol, result] = SteadyCom(EcCom, optionsTest);


for jSp = 1:4
    fprintf('X_%s:  %.6f\n', EcCom.infoCom.spAbbr{jSp}, result.BM(jSp));
end
disp(result);

iter = [0, result.iter0, NaN; result.iter];
for j = 0 : size(iter, 1)
    if j == 0
        fprintf('#iter\tgrowth rate (mu)\tmax. biomass (sum(X))\tmu * sum(X)\tmax. infeasibility\tguess method\n');
    else
        fprintf('%5d\t%16.6f\t%21.6f\t%11.6f\t%18.6e\t%d\n', iter(j,:))
    end
end
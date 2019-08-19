% FIXME: Work in progress
function [solution, totalFluxDiff, solStatus] = ...
  MOMAOneShot(model, origFlux, osenseStr, verbFlag, minNormFlag)
% Performs a quadratic version of the MOMA (minimization of
% metabolic adjustment) approach
%
% USAGE:
%
%    [solution, solutionWT, totalFluxDiff, solStatus] = MOMA(model, model, osenseStr, verbFlag, minNormFlag)
%
% INPUTS:
%    model:          Wild type model
%    origFlux:       Original Flux to approximate; not necessarily a viable flux vector
%
% OPTIONAL INPUTS:
%    osenseStr:        Maximize ('max') / minimize ('min') (Default = 'max')
%    verbFlag:         Verbose output (Default = false)
%    minNormFlag:      Work with minimum 1-norm flux distribution for the FBA
%                      problem (Default = false)
%
% OUTPUTS:
%    solution:      Deletion solution structure
%    solutionWT:       Wild-type solution structure
%    totalFluxDiff:    Value of the linear MOMA objective, i.e.
%                      :math:`\sum (v_{wt}-v_{del})^2`
%    solStatus:        Solution status - solves two different types of MOMA problems:
%
%                        1.  MOMA that avoids problems with alternative optima (this is the
%                            default)
%                        2.  MOMA that uses a minimum 1-norm wild type FBA solution (this approach
%                            is used if minNormFlag = true)
% First solve:
%
% .. math::
%      max ~&~ c_{wt}^T v_{wt0} \\
%          ~&~ lb_{wt} \leq v_{wt0} \leq ub_{wt} \\
%          ~&~ S_{wt}v_{wt0} = 0 \\
%
% Then solve:
%
% .. math::
%      min ~&~ \sum (v_{wt} - v_{del})^2 \\
%          ~&~ S_{wt}v_{wt} = 0 \\
%          ~&~ S_{del}v_{del} = 0 \\
%          ~&~ lb_{wt} \leq v_{wt} \leq ub_{wt} \\
%          ~&~ lb_{del} \leq v_{del} \leq ub_{del} \\
%          ~&~ c_{wt}^T v_{wt} = f_{wt} \\
%
% Here :math:`f_{wt}` is the optimal wild type objective value found by FBA in the
% first problem. Note that the FBA solution :math:`v_{wt0}` is not used in the second
% problem. This formulation avoids any problems with alternative optima
%
% First solve
%
% .. math::
%      max ~&~ c_{wt}^T v_{wt0} \\
%          ~&~ lb_{wt} \leq v_{wt0} \leq ub_{wt} \\
%          ~&~ S_{wt}v_{wt0} = 0 \\
%
% Then solve
%
% .. math::
%      min ~&~ |v_{wt}| \\
%          ~&~ S_{wt}v_{wt} = b_{wt} \\
%          ~&~ c_{wt}^T v_{wt} = f_{wt} \\
%          ~&~ lb_{wt} \leq v_{wt} \leq ub_{wt} \\
%
% Here :math:`f_{wt}` is the objective value obtained in the 1st optimization.
%
% Finally solve:
%
% .. math::
%      min ~&~ \sum (v_{wt} - v_{del})^2 \\
%          ~&~ S_{del}v_{del} = 0 \\
%          ~&~ lb_{del} \leq v_{del} \leq ub_{del}
%
% NOTE::
%
%    1) These formulation allows for selecting for more appropriate
%    optimal wild type FBA solutions as the starting point as opposed to
%    picking an arbitrary starting point (original MOMA implementation).
%
%    2) The reaction sets in the two models do not have to be equal as long as
%    there is at least one reaction in common
%
% .. Author: - Markus Herrgard 11/7/06

if (nargin <3 || isempty(osenseStr))
    osenseStr = 'max';
    if isfield(modelWT,'osenseStr')
        osenseStr = modelWT.osenseStr;
    end
end
if (nargin < 4)
    verbFlag = false;
end
if (nargin < 5)
    minNormFlag = false;
end

% LP solution tolerance
global CBT_LP_PARAMS
if (exist('CBT_LP_PARAMS', 'var'))
    if isfield(CBT_LP_PARAMS, 'objTol')
        tol = CBT_LP_PARAMS.objTol;
    else
        tol = 1e-6;
    end
else
    tol = 1e-6;
end

[nMets,nRxns] = size(model.S);

solution.f = [];
solution.x = [];
solution.stat = -1;

% Variables in the following problem are
% x = [v1;v2;delta]
% where v1 = wild type flux vector
%       v2 = deletion strain flux vector
%       delta = v1 - v2


if minNormFlag
  % TODO: 
  % QPproblem = buildLPproblemFromModel(model);
  % QPproblem.c(1:nRxns1) = -2*solutionWT.x;
  % QPproblem.F = sparse(size(QPproblem.A,2));
  % QPproblem.F(1:nRxns,1:nRxns) = 2*speye(nRxns);
else

  % Construct the LHS matrix
  % Rows:
  % 1: Swt*v1 = 0 for the wild type
  % 2: Sdel*v2 = 0 for the deletion strain
  % 5: c'v1 = f1 (wild type)
  LPDel = buildLPproblemFromModel(model);
  [nWTCtrs,nWTVars] = size(LPWT.A);
  [nDelCtrs,nDelVars] = size(LPDel.A);
  deltaMat = createDeltaMatchMatrix(modelWT.rxns,model.rxns);
  deltaMat = deltaMat(1:nCommon,1:(nRxns1+nRxns+nCommon));
  deltaMatWT = deltaMat(1:nCommon,1:nRxns1);
  deltaMatDel = deltaMat(1:nCommon,nRxns1+(1:nRxns));
  deltaMatCom = deltaMat(1:nCommon,(nRxns1+nRxns)+(1:nCommon));
  QPproblem.A = [LPWT.A, sparse(nWTCtrs,nDelVars+nCommon);...
                 sparse(nDelCtrs,nWTVars),LPDel.A,sparse(nDelCtrs,nCommon);...
                 deltaMatWT, sparse(nCommon,nWTVars - nRxns1), deltaMatDel, sparse(nCommon,nDelVars - nRxns), deltaMatCom;...
                 LPWT.c',sparse(1,nDelVars+nCommon)];
  % Construct the RHS vector
  QPproblem.b = [LPWT.b;LPDel.b;zeros(nCommon,1);objValWT];

  % Linear objective = 0
  QPproblem.c = zeros(nWTVars+nDelVars+nCommon,1);

  % Construct the ub/lb
  % delta [-10000 10000]
  QPproblem.lb = [LPWT.lb;LPDel.lb;-10000*ones(nCommon,1)];
  QPproblem.ub = [LPWT.ub;LPDel.ub;10000*ones(nCommon,1)];

  % Construct the constraint direction vector (G for delta's, E for
  % everything else)
  if (strcmp(osenseStr,'max'))
      csense = 'G';
  else
      csense = 'L';
  end

  QPproblem.csense = [LPWT.csense;LPDel.csense;repmat('E',nCommon,1);csense];        


  % F matrix
  QPproblem.F = [sparse(nWTVars+nDelVars,nWTVars+nDelVars+nCommon);
                 sparse(nCommon,nWTVars+nDelVars) 2*eye(nCommon)];

end

% in either case: minimize the distance
QPproblem.osense = 1;

if (verbFlag)
    fprintf('Solving MOMA: %d constraints %d variables ',size(QPproblem.A,1),size(QPproblem.A,2));
end

% Solve the linearMOMA problem    
%QPsolution = solveCobraQP(QPproblem,[],verbFlag-1);
QPsolution = solveCobraQP(QPproblem, 'printLevel', verbFlag-1, 'method', 0);

if (verbFlag)
    fprintf('%f seconds\n',QPsolution.time);
end

% Get the solution(s)
if QPsolution.stat == 1
    if minNormFlag
        solution.x = QPsolution.full;
    else
        solution.x = QPsolution.full((nRxns1+1):(nRxns1+nRxns));
        solutionWT.x = QPsolution.full(1:nRxns1);
    end
    solution.f = sum(model.c.*solution.x);
    totalFluxDiff = sum((solutionWT.x-solution.x).^2);
end
solution.stat = QPsolution.stat;
solStatus = QPsolution.stat;
solution.solver = QPsolution.solver;
solution.time = QPsolution.time;


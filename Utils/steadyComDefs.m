function options = steadyComDefs(multiModel, allGrowing)
% Convenience wrapper for running SteadyCom for our models
% INPUT: multiModel
% OUTPUT: [sol, result]

  allGrow = true;
  if nargin > 1
    allGrow = allGrowing;
  end

  options = struct();
  options.algorithm = 3;

% We're assuming proportial in this case (make a flag later)
%
nSpecies = length(multiModel.infoCom.spAbbr);
% options.BMrhs = [nSpecies];
% options.BMcsense = 'E';
% bmCon(1:nSpecies) = 1;
% options.BMcon = bmCon;

%
%  Somewhat confusingly, these are not fixing the individual X_i to be proportial
%  (althouch could be used to do so); BMcon = [1 1 1 ... 1], BMrhs = [1];
%  corresponds to the constraint in the paper Sum(Vbm_k) = u * Sum(X_k)
%
% * BMcon - Biomass constraint matrix :math:`(\sum (a_{ij} * X_j) </=/> b_i)`
%  (given as :math:`K * N_{organisms}` matrix for `K` constraints)
%   e.g. [0 1 1 0] for :math:`X_2 + X_3` in a 4-organisms model
%  * BMrhs - RHS for BMcon, `K x 1` vector for `K` constraints
%
% Requiring them all to sum to one, however, seems to be not restrictive enough:
% most species still won't grow. On the other hand, having this constraint with
% individual species constraints is apparently too restrictive to get biologically
% meaningful results (why?)
%
% options.BMcon = ones(1, nSpecies);
% options.BMrhs = [1];
% options.BMcsense = ['E'];
%
% So instead, we opt for setting individual small constraints,
% which seems to work well:

if allGrow
  disp("all grow");
  options.BMcon = diag(ones(nSpecies, 1));
  options.BMrhs = repmat(0.0001, nSpecies, 1)';
  options.BMcsense = [strjoin(repmat({'G'}, nSpecies, 1), '')];
  options.BMweight = 0.001;
  % options.feasCrit = 2;
end

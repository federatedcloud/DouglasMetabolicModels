function excIDs = findExcIDs(model)
% FINDS the exchange reaction IDs for a model.
% (Not using the given function in cobraToolbox because it includes
% equations not considered as exchange)
% In a multiple species model, finds the exchange reactions in the lumen
% ([u]) and not the individual exchange reactions
% ex. EX_co2[u] will be included, but not AF_IEX_co2[u]tr
%
% Author: Cindy Wu
%
  excRxns=startsWith(model.rxns,'EX_');
  excIDs=transpose(find(excRxns));
end

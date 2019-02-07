function multiModel = makeMultiModel(modelKeys, modelMap)
% makeMultiModel
%   modelKeys is a cell array of strings that index into modelMap;
%             these should be the model names.
%
%   modelMap is a containers.Map data structer, with key names as
%            indexes and models as values.

  modelsToSim = cellfun(@(k) modelMap(k), modelKeys, 'UniformOutput', false);

          % Adust so that SteadyCom/createMultiSpecies is happy %


  modelsToSim = cellfun(@(m) rmfield(m, 'metPubChemID'), modelsToSim, 'UniformOutput',false);
  multiModel = createMultipleSpeciesModel(modelsToSim, modelKeys);
  [multiModel.infoCom, multiModel.indCom] = getMultiSpeciesModelId(multiModel, modelKeys);

  origBioRxns = cellfun(@(m) m.rxns(find(m.c)), modelsToSim);
  bioRxns = cellfun(@(kn) strjoin(kn, ''), cellzip(modelKeys, origBioRxns), 'UniformOutput', false);
  bioRxnIds = findRxnIDs(multiModel, bioRxns);

  multiModel.infoCom.spBm = bioRxns;  % .spBm for organism biomass reactions
  multiModel.indCom.spBm = bioRxnIds;


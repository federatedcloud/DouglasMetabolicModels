function multiModel = makeMultiModel(modelKeys, modelMap)
% makeMultiModel
%   modelKeys is a cell array of strings that index into modelMap;
%             these should be the model names.
%
%   modelMap is a containers.Map data structer, with key names as
%            indexes and models as values.

  keysSz = size(modelKeys);
  if keysSz(1) == 1
    modelKeys = modelKeys';
    keysSz = size(modelKeys);
  end
  assert(keysSz(2) == 1);
  assert(length(keysSz) == 2);

  modelsToSim = cellFlatMap(@(k) modelMap(k), modelKeys);

          % Adust so that SteadyCom/createMultiSpecies is happy %
  try
    modelsToSim = cellFlatMap(@(m) rmfield(m, 'metPubChemID'), modelsToSim);
  catch
    warning('No field metPubChemID to remove.')
  end

  modelsToSim
  modelKeys
  multiModel = createMultipleSpeciesModel(modelsToSim, modelKeys);
  [multiModel.infoCom, multiModel.indCom] = getMultiSpeciesModelId(multiModel, modelKeys);

  origBioRxns = cellfun(@(m) m.rxns(find(m.c)), modelsToSim)
  bioRxns = cellFlatMap(@(kn) strjoin(kn, ''), cellzip(modelKeys, origBioRxns))
  bioRxnIds = findRxnIDs(multiModel, bioRxns)

  multiModel.c(bioRxnIds) = 1;
  multiModel.infoCom.spBm = bioRxns;  % .spBm for organism biomass reactions
  multiModel.indCom.spBm = bioRxnIds;


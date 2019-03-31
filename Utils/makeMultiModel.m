function multiModel = makeMultiModel(modelKeys, modelMap, mediaType)
% makeMultiModel
%   modelKeys is a cell array of strings that index into modelMap;
%             these should be the model names.
%
%   modelMap is a containers.Map data structer, with key names as
%            indexes and models as values.
%   mediaType: 'rich' (no action after createMultipleSpeciesModel is called),
%                     highly unconstrained
%              'minimal-merge': iteratively reapply constraints from constituent
%                     models; constraints from each model will overwrite consraints
%                     from other models in no particular order.
% Authors: Brandon Barker

  mediaTypeOptions = {'rich', 'minimal-merge', 'minimal-plus'};

  keysSz = size(modelKeys);
  if keysSz(1) == 1
    modelKeys = modelKeys';
    keysSz = size(modelKeys);
  end
  assert(keysSz(2) == 1);
  assert(length(keysSz) == 2);

  mediaTypeMsg = strcat(                         ...
    'Please specify a valid mediaTye; one of: ', ...
     strjoin(mediaTypeOptions, ', ')             ...
  );
  assert(length(intersect({mediaType}, mediaTypeOptions)) == 1, mediaTypeMsg);

  modelsToSim = cellFlatMap(@(k) modelMap(k), modelKeys);

          % Adust so that SteadyCom/createMultiSpecies is happy %
  try
    modelsToSim = cellFlatMap(@(m) rmfield(m, 'metPubChemID'), modelsToSim);
  catch
    warning('No field metPubChemID to remove.')
  end

  multiModel = createMultipleSpeciesModel(modelsToSim, modelKeys);
  [multiModel.infoCom, multiModel.indCom] = getMultiSpeciesModelId(multiModel, modelKeys);

  origBioRxns = cellfun(@(m) m.rxns(find(m.c)), modelsToSim);
  bioRxns = cellFlatMap(@(kn) strjoin(kn, ''), cellzip(modelKeys, origBioRxns));
  bioRxnIds = findRxnIDs(multiModel, bioRxns);

  multiModel.c(bioRxnIds) = 1;
  multiModel.infoCom.spBm = bioRxns;  % .spBm for organism biomass reactions
  multiModel.indCom.spBm = bioRxnIds;

  if strcmp(mediaType, 'minimal-merge')
    beforeMisses = findRestoreExcMisses(multiModel);
    multiModel = restoreExcBounds(multiModel, modelsToSim);
    afterMisses = findRestoreExcMisses(multiModel);
    if length(afterMisses) > 0
      s1 = sprintf('%d unrestored exchange reactions present before ', length(beforeMisses));
      s2 = sprintf('%d unrestored exchange reactions still present.\n', length(afterMisses));
      s3 = 'running restoreExcBounds.\n';
      warning([s1 s2 s3]);
      disp({'Reaction', 'Reaction Name'});
      disp({'--------------------------'});
      disp(horzcat(multiModel.rxns(afterMisses), multiModel.rxnNames(afterMisses)));
    end
  elseif strcmp(mediaType, 'minimal-plus')
    multiModel = createMinimalPlusModel(multiModel);
  end



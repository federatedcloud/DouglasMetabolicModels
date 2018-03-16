load('iNA875.mat')

model=rmfield(model,'metPubChemID');

iNA875=model;

load('iNA756.mat')

model=rmfield(model,'metPubChemID');

iNA756=model;

models{1,1}=iNA875;

models{2,1}=iNA756;

nameTagsModels{1,1}='iNA875_';

nameTagsModels{2,1}='iNA756_';

[modelJoint] = createMultipleSpeciesModel(models, nameTagsModels);

[sol, result]=SteadyCom(modelJoint,2)

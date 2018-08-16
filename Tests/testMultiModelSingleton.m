function testMultiModelSingleton( model_in )
%TESTMULTIMODELSINGLETON 
%   Detailed explanation goes here

in_fba_sol = optimizeCbModel(model_in);

% TODO: consider factoring this block out as another function:
multi_model =  createMultipleSpeciesModel({model_in}, {'Org1'});
[multi_model.infoCom, multi_model.indCom] = getMultiSpeciesModelId(multi_model, {'Org1'});
multi_model.csense = char('E' * ones(1,numel(multi_model.mets)));  % correct the csense
multi_model_corrected = restoreExcBounds(multi_model, {model_in});

multi_fba_sol = optimizeCbModel(multi_model_corrected);

% TODO: proper unit testing
%assertEquals(multi_fba_sol.obj, in_fba_sol.obj, 
%
testAssertion = multi_fba_sol.obj - in_fba_sol.obj < max(in_fba_sol.obj, multi_fba_sol.obj)/1000;
if ~testAssertion
    disp('multi model solution:');
    multi_fba_sol.obj
    disp('original solution:');
    in_fba_sol.obj
end    
assert(multi_fba_sol.obj - in_fba_sol.obj < max(in_fba_sol.obj, multi_fba_sol.obj)/1000)
end


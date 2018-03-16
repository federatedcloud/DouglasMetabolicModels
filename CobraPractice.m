model=readCbModel('iJN739.xml')
save iJN739.mat
writeCbModel(model,'xls','iJN739')
model= xls2model('iJN739.xls')
model= changeObjective(model,'BIOMASS_F1')
changeCobraSolver('glpk')
[massImbalance,imBalancedMass,imBalancedCharge,imBalancedBool,Elements]=checkMassChargeBalance(model,-1)
%missingMets={};
%presentMets={};
%[missingMets,presentMets]=biomassPrecursorCheck(model)
model=changeRxnBounds(model,ExchangeReactionName,-30,'l')
model=changeRxnBounds(model,ExchangeReactionName,0,'l')
model= changeRxnBounds(model, 'EX_glc_D(e)',-10,'l')
printConstraints(model,-1000,1000)
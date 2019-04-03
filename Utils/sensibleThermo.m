function multiModel = sensibleThermo(multiModel)
  forwardOnlyRxns = 
  multiModel = changeRxnBounds(multiModel, forwardOnlyRxns(ii), 0, 'l');
end

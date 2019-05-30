function effluxRxns = findEffluxRxns(multiModel)
  [selExc, selUpt] = findExcRxns(multiModel);
  effluxRxns = multiModel.rxns(selExc & (~selUpt));
  effluxRxns = filter1d(@(x) startsWith(x,'EX'), effluxRxns);
end

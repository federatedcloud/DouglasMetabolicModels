function [effluxRxns, influxRxns] = findEffluxInfluxRxns(multiModel)
  [selExc, selUpt] = findExcRxns(multiModel);
  effluxRxns = multiModel.rxns(selExc & (~selUpt));
  effluxRxns = filter1d(@(x) startsWith(x,'EX'), effluxRxns);
  influxRxns = multiModel.rxns(selUpt);
  influxRxns = filter1d(@(x) startsWith(x,'EX'), influxRxns);
end

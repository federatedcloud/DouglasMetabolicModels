function groupMap = readExRxnGroups()
  global DMMDIR;
  groupMap = readRxnGroups(strcat(                                ...
    DMMDIR, filesep, 'models', filesep, 'exchange_rxns_080719.csv' ...
  ));
end

function groupMap = readTrRxnGroups()
  global DMMDIR;
  groupMap = readRxnGroups(strcat(                                ...
    DMMDIR, filesep, 'models', filesep, 'u_trans_rxns_080719.csv' ...
  ));
end

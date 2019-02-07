

% taken from models/5.models.020419/media.xlsx

rxns = {
  'EX_glu-L(e)',
  'EX_ile-L(e)',
  'EX_leu-L(e)',
  'EX_met-L(e)',
  'EX_val-L(e)',
  'EX_nh4(e)',
  'EX_glc-D(e)',
  'EX_glyc(e)',
  'EX_h(e)',
  'EX_h2o(e)',
  'EX_o2(e)',
  'EX_pi(e)',
  'EX_so4(e)',
  'EX_h2s(e)',
  'EX_btn(e)',
  'EX_nac(e)',
  'EX_pnto-R(e)',
  'EX_pydam(e)',
  'EX_pydxn(e)',
  'EX_pydx5p(e)'
};

lower_bounds = {
  -5,
  -5,
  -5,
  -5,
  -5,
  -5,
  -10,
  -5,
  -1,
  -10,
  -20,
  -1,
  -5,
  -5,
  -1,
  -1,
  -1,
  -1,
  -1,
  -1
};

rxnLbMap = containers.Map(rxns, lower_bounds)

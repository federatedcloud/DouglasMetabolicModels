with import <nixpkgs> {};
let
  gurobiPlatform = "linux64";
  myGurobi = import (fetchurl {
    url = https://raw.githubusercontent.com/federatedcloud/COBRAContainers/035b0be9f720e77c95fcf6ef171a92c8fca4207c/nix/packages/gurobi/default.nix;
    hash = "sha256-VF5As00EVXNPQnakGPhrSyp44XFnP6b5Fg6JTs9Ql4c=";
   });
  hsMatlab = import (fetchurl {
    url = https://raw.githubusercontent.com/CornellCAC/haskell-matlab/4c57a51ccdb86a4747fb42b41e4bf5f4df1e023f/deps.nix;
    hash = "sha256-CLN++Y+E6a585oA6F0PM9I8Hbp5XgXofPsMASHFcvGc=";
   });
in
stdenv.mkDerivation {
  inherit myGurobi;
  inherit hsMatlab;
  name = "stack-matlab-env";
  buildInputs = [ stack zlib];
  libPath = [];
}

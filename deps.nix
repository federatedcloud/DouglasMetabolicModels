with import <nixpkgs> {};
let
  gurobiPlatform = "linux64";
  # convert to SRI hash with: nix to-sri --type sha256 <sha256-hash>
  myGurobi = import (fetchurl {
    url = https://raw.githubusercontent.com/federatedcloud/COBRAContainers/50786b7a64d2131c3f49096521a5374d315fc7e2/nix/packages/gurobi/default.nix;
    hash = "sha256-LW9h/Mlne7+D1iK0oQHDBzh7607TrHgZ4W3dydot+bw=";
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

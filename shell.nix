with import <nixpkgs> {};
let
  gurobiPlatform = "linux64";
  deps = (import ./deps.nix);
in
stdenv.mkDerivation {
  name = "stack-matlab-env";
  buildInputs = [ stack ];
  shellHook = ''
    export MATLAB_PATH=${deps.hsMatlab.matlabPath}
    export PATH=$PATH:$HOME/.local/bin:$MATLAB_PATH/bin

    source ${./patchMATLAB.sh}
    export GUROBI_HOME="${deps.myGurobi.out}/${gurobiPlatform}"
    export GUROBI_PATH="${deps.myGurobi.out}/${gurobiPlatform}"
    export GRB_LICENSE_FILE="/opt/gurobi_CAC.lic"
  '';
}

with import <nixpkgs> {}; {
  stackMatlabEnv = stdenv.mkDerivation {
    name = "stack-matlab-env";
    buildInputs = [ stack ];
    shellHook = ''
      export MATLAB_PATH=/opt/MATLAB/R2017a
      export PATH=$HOME/.local/bin:$MATLAB_PATH/bin:$PATH
    '';
  }; 
}

#!/usr/bin/env bash
if [[ -z "$MATLAB_PATH" ]]; then
    echo "Must set MATLAB_PATH to a particular MATLAB installation directory for build.sh." 1>&2
    exit 1
fi
stack --extra-lib-dirs=$MATLAB_PATH/bin/glnxa64 --extra-include-dirs=$MATLAB_PATH/extern/include --nix install

# Setup

After downloading via `git clone git@github.com:federatedcloud/DouglasMetabolicModels.git`,
go into the directory and run `git lfs install` (only once per repo) and `git lfs pull`
to pull in models and other data files.

Note: if using [CobraContainers](https://github.com/FederatedCLoud/CobraContainers),
you do **NOT** need to follow the [One-time-only for gurobi](https://github.com/FederatedCLoud/CobraContainers#one-time-only-for-gurobi)
instructions, but should do everything else mentioned under Nix (and Docker if using Nix in Docker).

Run `initDMM` from this (top-level directory) in MATLAB. This will
add paths to the environment.

Afterward, you still need to run `initCobraToolbox` from your `cobratoolbox` directory.

# Species

The following species are considered as part of the *Drosophila* gut microbiome, and the repository includes
models for these species:

* *Acetobacter* *fabarum*
* *Acetobacter* *pomorum*
* *Acetobacter* *tropicalis*
* *Lactobacillus* *brevis*
* *Lactobacillus* *plantarum*

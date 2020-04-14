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

Currently, the installed version of COBRA Toolbox doesn't work as it is required to be mutable, but once it does,
one could do `cd(getenv('COBRA_HOME'))` before running `initCobraToolbox(false)`.

If you get a git related error during the run of `initCobraToolbox` related to submodules,
try running the command outside fo MATLAB. Typically this is one of:

```
git submodule update --init --remote --no-fetch
```

# Species

The following species are considered as part of the *Drosophila* gut microbiome, and the repository includes
models for these species:

* *Acetobacter* *fabarum*
* *Acetobacter* *pomorum*
* *Acetobacter* *tropicalis*
* *Lactobacillus* *brevis*
* *Lactobacillus* *plantarum*


# Notes

When running any analysis that makes use of SteadyComFVA:

- Be sure to use a parpool if possible to speed things up.
- We suggest using the Gurobi solver, which seems to be over 1000x as fast as GLPK for our models.
- Do not change the paralell pool size if restarting a run (if you do, clear the checkpoint `.mat` files and start again.

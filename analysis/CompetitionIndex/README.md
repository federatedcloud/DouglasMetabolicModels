## Important note

The analysis has been integrated into the `CMP_and_CooperativeFluxes` analysis
due to algorithmic similarities, so see that folder for newer and more
comprehensive analyses.

## Description

The data generated here relies on the `competitionIndices` function.

It has the following header:  `Met, CompIdx, InfluxIdx, OutfluxIdx, FluxInfo`.

If the flux is < -1e-6 is is an Influx, if it is > 1e-6 it is an Outflux.

1. `Met` - The metabolite involved.
1. `CompIdx` - The net competition = `CompIdx` = `InfluxIdx` - `OutfluxIdx`.
1. `FluxInfo` - Per-species flux data associated with `Met`.


Each file corresponds to one simulation of the 5-member community
 - only printed the non-zero fluxes for each
organism; the reactions shown are all the transport reactions
for which there was flux in at least one of the organisms.
 - Each row is one metabolite in the model for which there is an exchange reaction with the environment and some nonzero flux into at least one species.

The Flux for the entire simulation is saved in the corresponding `*Flux.csv`.

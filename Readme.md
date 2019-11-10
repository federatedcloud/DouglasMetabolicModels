

# Notes

When running any analysis that makes use of SteadyComFVA:

- Be sure to use a parpool if possible to speed things up.
- We suggest using the Gurobi solver, which seems to be over 1000x as fast as GLPK for our models.
- Do not change the paralell pool size if restarting a run (if you do, clear the checkpoint `.mat` files and start again.

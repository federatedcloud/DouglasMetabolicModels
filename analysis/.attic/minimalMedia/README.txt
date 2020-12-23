# Creating a parallel pool

Before running the analysis, we need to create a pool, like so:

```matlab
mypool = parpool('local', 14, 'SpmdEnabled', false)
```

We don't need SPMD for inter-worker communication, if a worker dies, disabling it like
this makes the simulation more likely to finish. See note below on "pool size" for
why we have "14".

## Pool size

On a 28 core system with 220GB memory, we found good performance and memory usage
with a 14-process worker pool for MATLAB: sometimes a MATLAB process is seen to use
~2-3 cores at a time, and using many more processes will exhuast the memory on the system.

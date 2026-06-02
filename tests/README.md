# Tests

The tests are intentionally lightweight and run against the source files
directly. The repository does not need to be converted into an R package.

From the repository root:

```bash
source_conda
conda activate r_env_4.3.3
Rscript tests/run-tests.R
```

The suite covers representative statistical wrappers and one-worker runs of
the parallel entry points. Add a regression test whenever a helper bug is fixed.

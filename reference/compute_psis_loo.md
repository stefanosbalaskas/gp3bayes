# Compute PSIS-LOO for a gp3bayes Fit

Compute PSIS-LOO for a gp3bayes Fit

## Usage

``` r
compute_psis_loo(
  fit,
  moment_match = FALSE,
  reloo = FALSE,
  cores = 1L,
  save_psis = TRUE
)
```

## Arguments

- fit:

  A gp3bayes fit or `brmsfit`.

- moment_match:

  Whether to request moment matching.

- reloo:

  Whether to request exact refits for problematic observations.

- cores:

  Number of cores.

- save_psis:

  Whether to retain the PSIS object.

## Value

A conservative `gp3bayes_psis_loo` result.

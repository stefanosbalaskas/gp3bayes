# Compute PSIS-LOO from a Log-Likelihood Matrix

This function supports deterministic tests and advanced workflows that
already possess pointwise log-likelihood draws.

## Usage

``` r
compute_psis_loo_from_log_lik(
  log_lik,
  chain_id = NULL,
  cores = 1L,
  save_psis = TRUE
)
```

## Arguments

- log_lik:

  Matrix with posterior draws in rows and observations in columns.

- chain_id:

  Optional chain identifier for each row.

- cores:

  Number of cores.

- save_psis:

  Whether to retain the PSIS object.

## Value

A `gp3bayes_psis_loo`.

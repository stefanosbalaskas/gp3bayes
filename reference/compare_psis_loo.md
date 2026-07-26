# Compare Models with PSIS-LOO

Compare Models with PSIS-LOO

## Usage

``` r
compare_psis_loo(models, moment_match = FALSE, reloo = FALSE, cores = 1L)
```

## Arguments

- models:

  Named list of gp3bayes fits, `brmsfit` objects, or `gp3bayes_psis_loo`
  objects.

- moment_match:

  Whether to request moment matching.

- reloo:

  Whether to request exact refits for problematic observations.

- cores:

  Number of cores.

## Value

A `gp3bayes_loo_comparison`. The result never selects a model.

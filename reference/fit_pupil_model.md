# Fit a pupil model through the fixed rstan route

Fit a pupil model through the fixed rstan route

## Usage

``` r
fit_pupil_model(
  specification,
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = min(2L, chains),
  seed = 2026,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)
```

## Arguments

- specification:

  Approved pupil model specification.

- chains, iter, warmup, cores, seed:

  Sampling controls. Package-controlled cores are capped at two.

- adapt_delta, max_treedepth, refresh:

  Fixed safe sampling controls.

## Value

A `gp3bayes_pupil_fit`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

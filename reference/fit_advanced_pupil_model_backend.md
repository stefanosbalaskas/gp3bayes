# Fit an advanced pupil model through an approved brms backend

Fit an advanced pupil model through an approved brms backend

## Usage

``` r
fit_advanced_pupil_model_backend(
  specification,
  backend = c("rstan", "cmdstanr"),
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

  An advanced pupil specification.

- backend:

  `"rstan"` or `"cmdstanr"`.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted MCMC controls.

## Value

A `gp3bayes_pupil_advanced_fit` object.

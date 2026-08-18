# Fit an advanced pupil model using rstan

Fit an advanced pupil model using rstan

## Usage

``` r
fit_advanced_pupil_model(
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

  An advanced pupil specification.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted MCMC controls.

# Fit an advanced pupil model using cmdstanr

Fit an advanced pupil model using cmdstanr

## Usage

``` r
fit_advanced_pupil_model_cmdstanr(
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

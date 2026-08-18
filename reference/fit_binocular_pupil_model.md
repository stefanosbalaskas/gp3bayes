# Fit a joint binocular pupil model

Fit a joint binocular pupil model

## Usage

``` r
fit_binocular_pupil_model(
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

  A binocular specification.

- backend:

  rstan or cmdstanr.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted sampling controls.

# Fit the experimental nonlinear response-shape model

Fit the experimental nonlinear response-shape model

## Usage

``` r
fit_pupil_response_shape_model(
  specification,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2500L,
  warmup = 1250L,
  cores = min(2L, chains),
  seed = 2026,
  adapt_delta = 0.97,
  max_treedepth = 13L,
  refresh = 0L
)
```

## Arguments

- specification:

  A response-shape specification.

- backend:

  rstan or cmdstanr.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted sampling controls.

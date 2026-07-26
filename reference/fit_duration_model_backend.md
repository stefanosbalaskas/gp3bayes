# Fit a Duration Model with a Selected Full-MCMC Backend

Fit a Duration Model with a Selected Full-MCMC Backend

## Usage

``` r
fit_duration_model_backend(
  specification,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = .gp3b_default_cores(chains),
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)

fit_duration_model_cmdstanr(
  specification,
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = .gp3b_default_cores(chains),
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)
```

## Arguments

- specification:

  An approved duration model specification.

- backend:

  Either `"rstan"` or `"cmdstanr"`.

- chains:

  Number of chains.

- iter:

  Total iterations per chain.

- warmup:

  Warmup iterations per chain.

- cores:

  Number of cores, not exceeding chains.

- seed:

  Non-negative integer seed.

- adapt_delta:

  NUTS target acceptance probability.

- max_treedepth:

  NUTS maximum tree depth.

- refresh:

  Progress refresh interval.

## Value

A `gp3bayes_backend_portable_fit`.

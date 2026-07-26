# Fit a Binary Model with a Selected Full-MCMC Backend

The formula and family remain contract-restricted. The only selectable
implementation detail is whether brms delegates full MCMC sampling to
rstan or CmdStanR.

## Usage

``` r
fit_binary_model_backend(
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

fit_binary_model_cmdstanr(
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

  An approved binary model specification.

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

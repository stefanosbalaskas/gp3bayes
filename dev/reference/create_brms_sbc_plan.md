# Create a brms-Based Simulation-Based Calibration Plan

Constructs a complete SBC generator/backend pair using the restricted
gp3bayes brms translation. Because the generator and backend share brms
implementation code, this plan is mainly a computational-calibration
check; custom independent generators remain preferable for detecting
shared implementation errors.

## Usage

``` r
create_brms_sbc_plan(
  specification,
  n_sims = 20L,
  backend = c("rstan", "cmdstanr"),
  chains = 2L,
  iter = 1000L,
  warmup = 500L,
  thin = 1L,
  seed = 1L,
  generator_iter = 3000L,
  generator_warmup = 2000L
)
```

## Arguments

- specification:

  An approved binary or duration specification.

- n_sims:

  Number of simulated datasets.

- backend:

  Either rstan or cmdstanr.

- chains:

  Number of chains used per SBC fit.

- iter:

  Total iterations per SBC fit.

- warmup:

  Warmup iterations.

- thin:

  Thinning interval.

- seed:

  Seed used when datasets are generated.

- generator_iter:

  Total prior-only generator iterations.

- generator_warmup:

  Prior-only generator warmup.

## Value

A `gp3bayes_sbc_plan`.

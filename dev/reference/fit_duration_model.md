# Fit an Approved Hierarchical Lognormal Duration Model

Fits an approved strictly positive uncensored duration model using full
MCMC through the fixed `brms` and `rstan` route.

## Usage

``` r
fit_duration_model(
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

  A `gp3bayes_duration_model_specification`.

- chains:

  Number of MCMC chains.

- iter:

  Total iterations per chain.

- warmup:

  Warmup iterations per chain.

- cores:

  Processor cores, not exceeding `chains`.

- seed:

  Non-negative integer seed.

- adapt_delta:

  Target NUTS acceptance probability.

- max_treedepth:

  Maximum NUTS tree depth.

- refresh:

  Console progress refresh interval.

## Value

A `gp3bayes_duration_fit`.

## Details

The likelihood is fixed to lognormal, the link to identity on the
mean-log parameter, the interface to `brms`, the backend to `rstan`, and
the algorithm to full MCMC sampling. A returned fit does not establish
convergence or posterior adequacy.

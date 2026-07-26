# Fit an Approved Hierarchical Binary Model

Fits an approved binary model specification using full MCMC sampling
through the fixed `brms` and `rstan` route.

## Usage

``` r
fit_binary_model(
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

  A `gp3bayes_binary_model_specification`.

- chains:

  Number of MCMC chains.

- iter:

  Total iterations per chain, including warmup.

- warmup:

  Warmup iterations per chain.

- cores:

  Number of processor cores. It cannot exceed `chains`.

- seed:

  Non-negative integer random-number seed.

- adapt_delta:

  Target acceptance probability for the No-U-Turn sampler.

- max_treedepth:

  Maximum tree depth for the No-U-Turn sampler.

- refresh:

  Console progress refresh interval. Use zero to suppress iteration
  progress output.

## Value

A `gp3bayes_binary_fit` containing the fitted backend object, original
specification, restricted translation, and recorded sampling settings.

## Details

The function fixes the likelihood to Bernoulli, the link to logit, the
interface to `brms`, the sampling backend to `rstan`, and the algorithm
to full MCMC sampling. It does not expose arbitrary backend arguments.

A returned fit is not evidence of convergence, posterior adequacy,
causal identification, or substantive validity. Those assessments
require separate diagnostic and reporting gates.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_binary_model(
  specification,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 2026
)
} # }
```

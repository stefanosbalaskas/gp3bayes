# Check the approved pupil priors predictively

Creates a governed prior-predictive plan by default. With
`execute = TRUE`, draws from the prior-only approved Gaussian `brms`
model and compares replicated pupil values with the observed model-scale
range. The check reports evidence only and never changes priors
automatically.

## Usage

``` r
check_pupil_prior_predictive(
  specification,
  execute = FALSE,
  backend = c("rstan", "cmdstanr"),
  draws = 200L,
  chains = 2L,
  iter = 1000L,
  warmup = 500L,
  cores = min(2L, chains),
  seed = 2026,
  probability = 0.95,
  max_cells = 3000000L
)
```

## Arguments

- specification:

  Approved pupil model specification.

- execute:

  Whether to run prior-only MCMC. Defaults to `FALSE`.

- backend:

  Approved backend, `"rstan"` or `"cmdstanr"`.

- draws:

  Number of prior predictive replicated draws to retain.

- chains, iter, warmup, cores, seed:

  Sampling controls. Package-controlled cores are capped at two.

- probability:

  Central predictive interval probability.

- max_cells:

  Maximum retained draw-by-observation cells.

## Value

A `gp3bayes_pupil_prior_predictive` evidence object.

## Governance boundary

This operation does not tune priors, select a favourable prior scale, or
certify a model as scientifically adequate. `execute = FALSE` performs
no compilation or fitting.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

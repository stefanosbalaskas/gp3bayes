# Simulate Marginal Draws from Declared gp3bayes Priors

Simulate Marginal Draws from Declared gp3bayes Priors

## Usage

``` r
simulate_declared_prior_draws(
  x,
  variables = NULL,
  regex = NULL,
  ndraws = 4000L,
  seed = 1L
)
```

## Arguments

- x:

  A gp3bayes fit, specification, or prior specification.

- variables:

  Posterior-style variable names. When `x` is a fitted model, supported
  variables can be inferred.

- regex:

  Optional regular expression applied after inference.

- ndraws:

  Number of marginal prior draws.

- seed:

  Non-negative integer seed.

## Value

A numeric draw matrix.

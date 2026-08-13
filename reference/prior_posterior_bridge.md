# Create a Declared-Prior versus Posterior Bridge

Compares marginal draws from the recorded gp3bayes prior specification
with fitted posterior draws on the same parameter scale.

## Usage

``` r
prior_posterior_bridge(
  fit,
  variables = NULL,
  regex = "^(b_|sd_|cor_|sigma$)",
  ndraws = 4000L,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional exact posterior variables.

- regex:

  Optional posterior-variable regular expression.

- ndraws:

  Number of prior draws and maximum posterior draws used.

- probs:

  Three interval probabilities.

- seed:

  Non-negative integer seed.

## Value

A `gp3bayes_prior_posterior_bridge`.

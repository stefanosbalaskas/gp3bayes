# Check Binary Posterior Predictive Behaviour

Compares observed binary summaries with replicated outcomes from the
fitted posterior predictive distribution.

## Usage

``` r
check_binary_posterior_predictive(
  fit,
  draws = 500,
  seed = 1,
  pass_probability = 0.8,
  review_probability = 0.95
)
```

## Arguments

- fit:

  A `gp3bayes_binary_fit`.

- draws:

  Number of posterior predictive replications.

- seed:

  Non-negative integer seed used to select predictive draws.

- pass_probability:

  Central predictive interval used for a pass.

- review_probability:

  Wider central predictive interval used for review.

## Value

A `gp3bayes_binary_posterior_predictive_check`.

## Details

The check evaluates prespecified descriptive summaries. It does not
prove that the likelihood, link, random-effects structure, or
substantive model is adequate.

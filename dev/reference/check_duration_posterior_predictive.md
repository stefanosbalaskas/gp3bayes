# Check Duration Posterior Predictive Behaviour

Compares observed positive-duration summaries with replicated outcomes
from the fitted posterior predictive distribution.

## Usage

``` r
check_duration_posterior_predictive(
  fit,
  draws = 500L,
  seed = 1L,
  pass_probability = 0.8,
  review_probability = 0.95
)
```

## Arguments

- fit:

  A `gp3bayes_duration_fit`.

- draws:

  Number of posterior predictive data sets.

- seed:

  Non-negative integer seed.

- pass_probability:

  Central predictive interval used for pass.

- review_probability:

  Wider predictive interval used for review.

## Value

A `gp3bayes_duration_posterior_predictive_check`.

## Details

The check covers median, mean, upper-tail, dispersion, condition-ratio,
and grouping summaries. It does not prove global model adequacy.

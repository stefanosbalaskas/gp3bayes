# Score posterior predictive draws against observed pupil values

Metrics are descriptive out-of-sample or held-out scores only when the
caller supplies predictions generated without using the scored
observations.

## Usage

``` r
score_pupil_predictions(observed, draws, probability = 0.9)
```

## Arguments

- observed:

  Numeric observed values.

- draws:

  Matrix of posterior predictive draws (draws x observations).

- probability:

  Central interval used for empirical coverage and width.

## Value

A `gp3bayes_pupil_predictive_score` object.

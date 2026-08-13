# Posterior Prediction Contrast

Posterior Prediction Contrast

## Usage

``` r
prediction_contrast(
  x,
  row1,
  row2,
  measure = c("difference", "ratio", "odds_ratio"),
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- row1, row2:

  Two prediction rows to compare.

- measure:

  `"difference"`, `"ratio"`, or `"odds_ratio"`.

- probs:

  Posterior interval probabilities.

## Value

A one-row posterior contrast summary.

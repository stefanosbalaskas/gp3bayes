# Pairwise Prediction Contrasts

Pairwise Prediction Contrasts

## Usage

``` r
prediction_pairwise_contrasts(
  x,
  rows = NULL,
  measure = c("difference", "ratio"),
  max_rows = 20L,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- rows:

  Optional prediction-row indices. At most `max_rows` rows may be
  compared.

- measure:

  Difference or ratio.

- max_rows:

  Maximum number of prediction rows allowed.

- probs:

  Three posterior interval probabilities.

## Value

A data frame containing every unique pairwise contrast.

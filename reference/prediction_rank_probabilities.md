# Posterior Ranking Probabilities for Prediction Rows

Summarises relative ordering among a small, explicitly supplied set of
prediction rows. No row is automatically selected or declared superior.

## Usage

``` r
prediction_rank_probabilities(
  x,
  rows = NULL,
  direction = c("higher", "lower"),
  max_rows = 20L
)
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- rows:

  Optional prediction rows.

- direction:

  Whether larger or smaller values receive rank 1.

- max_rows:

  Maximum rows that may be ranked.

## Value

A descriptive ranking-probability table.

# Convert Prediction Draws to Long Form

Convert Prediction Draws to Long Form

## Usage

``` r
prediction_draws_long(x, max_draws = NULL, seed = 1L)
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- max_draws:

  Optional maximum number of posterior draws retained.

- seed:

  Non-negative integer used only if draws are subsampled.

## Value

A long data frame with draw, observation, and predicted value.

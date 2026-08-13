# Duration Predictive Q-Q Table

Duration Predictive Q-Q Table

## Usage

``` r
duration_qq_table(x, probs = seq(0.05, 0.95, by = 0.05))
```

## Arguments

- x:

  A duration posterior predictive `gp3bayes_prediction`.

- probs:

  Quantile probabilities.

## Value

A quantile-comparison table.

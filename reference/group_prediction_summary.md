# Group Prediction Summary

Aggregates posterior prediction draws over one or more columns already
present in the prediction data.

## Usage

``` r
group_prediction_summary(x, by, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- by:

  Character vector naming grouping columns in `x$newdata`.

- probs:

  Three probabilities for group-level posterior intervals.

## Value

A group-level posterior prediction table.

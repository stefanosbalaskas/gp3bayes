# Review Extreme Positive Durations Without Deleting Them

Flags observations that are extreme on the log-duration scale using both
a robust MAD rule and an outer-IQR rule. The function never deletes
values and never changes the model family automatically.

## Usage

``` r
review_duration_extremes(data, contract, mad_cutoff = 4, iqr_multiplier = 3)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved duration contract.

- mad_cutoff:

  Robust absolute z-score cutoff on log durations.

- iqr_multiplier:

  Multiplier for the outer-IQR rule on log durations.

## Value

A `gp3bayes_duration_extreme_review` object.

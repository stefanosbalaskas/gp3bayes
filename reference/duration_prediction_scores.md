# Duration Prediction Scores

Duration Prediction Scores

## Usage

``` r
duration_prediction_scores(x, observed = NULL)
```

## Arguments

- x:

  A duration prediction object or positive numeric predictions.

- observed:

  Optional positive observed durations.

## Value

A one-row table of absolute and squared prediction errors on the
response and log scales.

## Examples

``` r
duration_prediction_scores(c(100, 120, 90), c(110, 115, 100))
#>   n      mae     rmse median_absolute_error    log_mae   log_rmse
#> 1 3 8.333333 8.660254                    10 0.08107677 0.08562747
#>   mean_log_error automatic_decision
#> 1    -0.05270369              FALSE
```

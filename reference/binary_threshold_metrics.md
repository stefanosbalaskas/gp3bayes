# Binary Threshold-Metric Curve

Binary Threshold-Metric Curve

## Usage

``` r
binary_threshold_metrics(
  x,
  observed = NULL,
  thresholds = seq(0.1, 0.9, by = 0.05)
)
```

## Arguments

- x:

  A binary expected-response prediction or numeric probabilities.

- observed:

  Optional observed binary outcomes.

- thresholds:

  Numeric thresholds between 0 and 1, inclusive.

## Value

A data frame with accuracy, sensitivity, specificity, and balanced
accuracy over the supplied thresholds.

## Examples

``` r
binary_threshold_metrics(
  c(0.1, 0.8, 0.7, 0.2),
  c(0, 1, 1, 0),
  thresholds = c(0.3, 0.5, 0.7)
)
#>   n brier  log_loss auc threshold accuracy sensitivity specificity
#> 1 4 0.045 0.2270806   1       0.3        1           1           1
#> 2 4 0.045 0.2270806   1       0.5        1           1           1
#> 3 4 0.045 0.2270806   1       0.7        1           1           1
#>   balanced_accuracy automatic_decision
#> 1                 1              FALSE
#> 2                 1              FALSE
#> 3                 1              FALSE
```

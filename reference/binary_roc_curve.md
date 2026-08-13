# Binary ROC Curve

Binary ROC Curve

## Usage

``` r
binary_roc_curve(x, observed = NULL, thresholds = NULL)
```

## Arguments

- x:

  A binary expected prediction or numeric probabilities.

- observed:

  Optional observed binary outcomes.

- thresholds:

  Optional thresholds. By default all finite empirical breakpoints are
  used.

## Value

A data frame containing false-positive and true-positive rates.

## Examples

``` r
binary_roc_curve(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#>   threshold false_positive_rate true_positive_rate
#> 1       Inf                 0.0                0.0
#> 2       0.8                 0.0                0.5
#> 3       0.7                 0.0                1.0
#> 4       0.2                 0.5                1.0
#> 5       0.1                 1.0                1.0
#> 6      -Inf                 1.0                1.0
```

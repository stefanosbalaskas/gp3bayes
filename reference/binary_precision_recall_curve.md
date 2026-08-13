# Binary Precision-Recall Curve

Binary Precision-Recall Curve

## Usage

``` r
binary_precision_recall_curve(x, observed = NULL, thresholds = NULL)
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

A data frame containing recall and precision.

## Examples

``` r
binary_precision_recall_curve(
  c(0.1, 0.8, 0.7, 0.2),
  c(0, 1, 1, 0)
)
#>   threshold recall precision
#> 1       Inf    0.0 1.0000000
#> 2       0.8    0.5 1.0000000
#> 3       0.1    1.0 0.5000000
#> 4      -Inf    1.0 0.5000000
#> 5       0.2    1.0 0.6666667
#> 6       0.7    1.0 1.0000000
```

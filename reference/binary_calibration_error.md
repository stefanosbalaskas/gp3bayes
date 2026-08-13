# Binary Calibration Error

Binary Calibration Error

## Usage

``` r
binary_calibration_error(x, observed = NULL, bins = 10L)
```

## Arguments

- x:

  A binary expected prediction or numeric probabilities.

- observed:

  Optional observed outcomes.

- bins:

  Number of equal-frequency bins.

## Value

A one-row table with expected and maximum absolute calibration error.

## Examples

``` r
binary_calibration_error(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0), bins = 2)
#>   n bins_requested bins_used expected_calibration_error
#> 1 4              2         2                        0.2
#>   maximum_calibration_error automatic_adequacy_verdict
#> 1                      0.25                      FALSE
```

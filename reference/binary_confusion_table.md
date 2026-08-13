# Binary Confusion Table

Binary Confusion Table

## Usage

``` r
binary_confusion_table(x, observed = NULL, threshold = 0.5)
```

## Arguments

- x:

  A binary expected prediction or numeric probabilities.

- observed:

  Optional observed binary outcomes.

- threshold:

  Classification threshold from 0 to 1.

## Value

A four-row confusion table plus rates as attributes.

## Examples

``` r
binary_confusion_table(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#>   observed predicted count threshold
#> 1        0         0     2       0.5
#> 2        0         1     0       0.5
#> 3        1         0     0       0.5
#> 4        1         1     2       0.5
```

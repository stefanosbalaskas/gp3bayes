# Compare Estimand Sensitivity Across Alternative Fits

Compare Estimand Sensitivity Across Alternative Fits

## Usage

``` r
compare_estimand_sensitivity(
  reference,
  alternatives,
  quantity = reference$primary_quantity
)
```

## Arguments

- reference:

  Reference `gp3bayes_estimand`.

- alternatives:

  Named list of alternative estimands.

- quantity:

  Quantity to compare; defaults to the reference primary quantity.

## Value

A `gp3bayes_estimand_sensitivity`.

# Audit Estimand Invariance Against a Declared Tolerance

Audit Estimand Invariance Against a Declared Tolerance

## Usage

``` r
audit_estimand_invariance(
  reference,
  alternative,
  quantity = reference$primary_quantity,
  tolerance
)
```

## Arguments

- reference, alternative:

  Comparable gp3bayes estimands.

- quantity:

  Quantity to compare.

- tolerance:

  Maximum absolute median difference considered invariant for the
  declared scientific use.

## Value

A `gp3bayes_estimand_invariance_audit`.

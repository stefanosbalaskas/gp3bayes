# Audit Duration-Unit Invariance

Checks the unit-free median and predictive-quantile ratios and the
expected scaling of absolute predictive quantities after a known unit
conversion.

## Usage

``` r
audit_duration_unit_invariance(
  reference,
  converted,
  multiplier,
  tolerance = 0.02
)
```

## Arguments

- reference, converted:

  Comparable duration estimands.

- multiplier:

  Conversion factor applied to the outcome unit.

- tolerance:

  Absolute tolerance for unit-free ratios and relative tolerance for
  scaled absolute quantities.

## Value

A `gp3bayes_duration_unit_invariance_audit`.

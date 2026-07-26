# Create a Duration-Unit Sensitivity Specification

Re-expresses an approved prepared duration outcome in a new unit by a
positive multiplicative conversion, shifts the baseline median
accordingly, and retains all dimensionless prior scales.

## Usage

``` r
create_duration_unit_sensitivity_specification(
  specification,
  multiplier,
  new_unit
)
```

## Arguments

- specification:

  An approved duration specification.

- multiplier:

  Positive conversion factor from the current analysis unit to the new
  unit.

- new_unit:

  New non-empty unit label.

## Value

An approved duration sensitivity specification.

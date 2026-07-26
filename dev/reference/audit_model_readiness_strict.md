# Run a Strict Model-Readiness Audit

Extends
[`audit_model_readiness()`](https://stefanosbalaskas.github.io/gp3bayes/dev/reference/audit_model_readiness.md)
with explicit overall condition balance, binary within-group outcome
variation, identifier-like predictor review, fixed-effects rank,
duration extremes and boundaries, and optional binary separation
screening.

## Usage

``` r
audit_model_readiness_strict(
  data,
  contract,
  condition_warning_fraction = 0.1,
  condition_failure_fraction = 0.02,
  identifier_unique_fraction = 0.9,
  duration_allowed_range = NULL,
  censor_col = NULL,
  run_separation = TRUE
)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved model contract.

- condition_warning_fraction, condition_failure_fraction:

  Condition balance thresholds.

- identifier_unique_fraction:

  Identifier-like uniqueness threshold.

- duration_allowed_range:

  Optional positive duration bounds.

- censor_col:

  Optional duration censoring indicator.

- run_separation:

  Whether to run the optional fixed-effects separation screen when the
  family is binary.

## Value

A `gp3bayes_strict_readiness_audit` object.

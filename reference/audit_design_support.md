# Audit Overall Design Support

Combines missingness, fixed-effects design, random-effects support,
standard readiness, and optional binary separation screening into one
pre-fit audit.

## Usage

``` r
audit_design_support(
  x,
  contract = NULL,
  separation = FALSE,
  strict_readiness = TRUE
)
```

## Arguments

- x:

  A data frame, prepared object, model specification, or fit.

- contract:

  Required when `x` is a raw data frame.

- separation:

  Whether to run
  [`detect_binary_separation()`](https://stefanosbalaskas.github.io/gp3bayes/reference/detect_binary_separation.md)
  when the contract is binary and `detectseparation` is installed.

- strict_readiness:

  Whether to include
  [`audit_model_readiness_strict()`](https://stefanosbalaskas.github.io/gp3bayes/reference/audit_model_readiness_strict.md).

## Value

A `gp3bayes_design_support_audit`.

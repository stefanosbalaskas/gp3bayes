# Preflight an Approved Model Specification

Convenience wrapper around
[`audit_design_support()`](https://stefanosbalaskas.github.io/gp3bayes/reference/audit_design_support.md)
for an already-created specification.

## Usage

``` r
preflight_model_specification(specification, ...)
```

## Arguments

- specification:

  A gp3bayes model specification.

- ...:

  Arguments passed to
  [`audit_design_support()`](https://stefanosbalaskas.github.io/gp3bayes/reference/audit_design_support.md).

## Value

A `gp3bayes_design_support_audit`.

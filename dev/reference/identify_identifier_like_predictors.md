# Identify Identifier-Like Numeric Predictors

Applies conservative heuristics to declared numeric predictors. A
flagged predictor is a review signal only; explicit declaration in a
contract is never silently overridden.

## Usage

``` r
identify_identifier_like_predictors(
  data,
  contract,
  unique_fraction = 0.9,
  integer_fraction = 0.98,
  monotone_correlation = 0.98
)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved model contract.

- unique_fraction:

  Fraction of rows that must be unique before a predictor can be
  considered identifier-like.

- integer_fraction:

  Fraction of finite values that must be integer-like.

- monotone_correlation:

  Absolute correlation with row order used as a heuristic for
  sequence-like identifiers.

## Value

A `gp3bayes_identifier_predictor_audit` object.

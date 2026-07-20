# Validate a Prior Specification

Validates the completeness and internal consistency of a
`gp3bayes_prior_specification`.

## Usage

``` r
validate_prior_specification(priors, contract = NULL)
```

## Arguments

- priors:

  A `gp3bayes_prior_specification` created by
  [`create_prior_specification()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_prior_specification.md).

- contract:

  Optional `gp3bayes_model_contract`.

## Value

`priors`, invisibly.

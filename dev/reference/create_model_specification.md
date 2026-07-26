# Create a Complete Model Specification

Combines a model contract, a successful readiness audit, an approved
formula, and a validated prior specification into one
backend-independent model specification.

## Usage

``` r
create_model_specification(contract, audit, priors)
```

## Arguments

- contract:

  A `gp3bayes_model_contract`.

- audit:

  A `gp3bayes_readiness_audit`.

- priors:

  A `gp3bayes_prior_specification`.

## Value

An object of class `gp3bayes_model_specification`.

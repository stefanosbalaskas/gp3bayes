# Translate a Duration Model Specification to brms

Converts an approved backend-independent duration specification into a
fixed hierarchical lognormal `brms` representation.

## Usage

``` r
translate_duration_model_to_brms(specification)
```

## Arguments

- specification:

  A `gp3bayes_duration_model_specification`.

## Value

A `gp3bayes_duration_backend_specification`.

## Details

Translation validates the formula and priors but does not compile Stan
code or fit a model. Users cannot supply an alternative family, formula,
backend, algorithm, Stan extension, or arbitrary backend arguments.

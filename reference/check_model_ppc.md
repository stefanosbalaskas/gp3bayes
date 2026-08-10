# Check Posterior Predictive Behaviour

Family-neutral wrapper around the approved family-specific posterior
predictive checks. Passing this check is not a global adequacy claim.

## Usage

``` r
check_model_ppc(fit, ...)
```

## Arguments

- fit:

  A `gp3bayes_fit`.

- ...:

  Family-specific diagnostic arguments.

## Value

A family-specific `gp3bayes_posterior_predictive_check`.

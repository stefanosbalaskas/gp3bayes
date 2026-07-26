# Translate a Binary Model Specification to brms

Converts an approved backend-independent binary specification into a
restricted `brms` representation. The formula, Bernoulli-logit family,
and priors are derived entirely from the existing `gp3bayes`
specification.

## Usage

``` r
translate_binary_model_to_brms(specification)
```

## Arguments

- specification:

  A `gp3bayes_binary_model_specification`.

## Value

A `gp3bayes_binary_backend_specification` containing the restricted
formula, family, translated priors, validated prior table, and backend
metadata.

## Details

This function performs translation and prior validation only. It does
not compile Stan code, run MCMC, create posterior draws, or assess
convergence. Users cannot supply an alternative formula, family,
backend, algorithm, or arbitrary backend arguments.

## Examples

``` r
if (requireNamespace("brms", quietly = TRUE)) {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd = 0,
    seed = 2026
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition"
  )

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c("control", "treatment")
  )

  specification <- specify_binary_model(
    prepared,
    baseline = 0.35
  )

  translate_binary_model_to_brms(specification)
}
#> <gp3bayes_binary_backend_specification>
#>   Formula: selected ~ condition + (1 | participant_id) + (1 | item_id)
#>   Family: Bernoulli-logit
#>   Interface: brms
#>   Sampling backend: rstan
#>   Algorithm: sampling
#>   Backend available: TRUE
#>   Compiled: FALSE
#>   Fit performed: FALSE
```

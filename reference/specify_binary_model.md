# Specify a Backend-Independent Binary Model

Combines prepared binary data, a successful readiness audit, the
restricted hierarchical formula, and validated family-specific priors.
The returned object is not executable and performs no model fitting.

## Usage

``` r
specify_binary_model(
  prepared,
  baseline = 0.5,
  intercept_scale = 1.5,
  coefficient_scale = 0.75,
  group_sd_scale = 1,
  correlation_eta = 2,
  student_df = 3
)
```

## Arguments

- prepared:

  A `gp3bayes_binary_prepared` object.

- baseline:

  Plausible baseline event probability.

- intercept_scale:

  Optional scale for the normal intercept prior.

- coefficient_scale:

  Optional common scale for normal population-level coefficient priors,
  including the approved interaction.

- group_sd_scale:

  Scale for half-Student-t group standard deviations.

- correlation_eta:

  LKJ shape used when a random slope is requested.

- student_df:

  Degrees of freedom for half-Student-t scale priors.

## Value

A `gp3bayes_binary_model_specification` that also inherits from
`gp3bayes_model_specification`.

## Details

The specification retains the prepared data because backend-independent
prior predictive simulation must reproduce the declared design. It
contains no backend object, posterior draws, or fitted model.

## Examples

``` r
simulation <- simulate_hierarchical_binary_data(
  n_participants = 12,
  trials_per_participant = 8,
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

specification
#> <gp3bayes_binary_model_specification>
#>   Formula: selected ~ condition + (1 | participant_id) + (1 | item_id)
#>   Fixed formula: selected ~ condition
#>   Baseline probability: 0.35
#>   Readiness: ready_with_warnings
#>   Fitting engine: none
#>   Backend dependency: none
#>   Fit performed: FALSE
```

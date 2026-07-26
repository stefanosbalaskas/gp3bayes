# Check Binary Prior Predictive Behaviour

Simulates replicated binary outcomes from the declared prior
specification and prepared design without calling a Bayesian fitting
backend.

## Usage

``` r
check_binary_prior_predictive(
  specification,
  draws = 500,
  seed = 1,
  plausible_rate = c(0.01, 0.99),
  boundary_probability = c(0.01, 0.99),
  extreme_contrast = 0.8,
  maximum_degenerate_participant_fraction = 0.5,
  maximum_boundary_mass = 0.5,
  maximum_extreme_probability = 0.25
)
```

## Arguments

- specification:

  A `gp3bayes_binary_model_specification`.

- draws:

  Number of prior predictive data sets.

- seed:

  Non-negative integer random-number seed.

- plausible_rate:

  Increasing lower and upper limits for plausible overall and
  condition-specific event rates.

- boundary_probability:

  Probability thresholds used to identify prior mass close to zero and
  one.

- extreme_contrast:

  Absolute probability-scale condition contrast considered extreme.

- maximum_degenerate_participant_fraction:

  Maximum participant fraction allowed to have all-zero or all-one
  replicated outcomes in a draw.

- maximum_boundary_mass:

  Maximum fraction of row probabilities allowed beyond the declared
  boundary thresholds in a draw.

- maximum_extreme_probability:

  Maximum acceptable fraction of prior predictive draws violating each
  criterion.

## Value

A `gp3bayes_binary_prior_predictive_check` containing replicated
summaries, structured checks, thresholds, and the seed.

## Details

Failure does not select or alter priors automatically. It indicates that
the declared priors and design generate outcomes that require
substantive review. This check assesses prior implications, not
posterior adequacy or model fit.

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

check_binary_prior_predictive(
  specification,
  draws = 100,
  seed = 2027
)
#> <gp3bayes_binary_prior_predictive_check>
#>   Adequate: TRUE
#>   Draws: 100
#>   Failed checks: 0
#>   Backend: none
#>   Fit performed: FALSE
```

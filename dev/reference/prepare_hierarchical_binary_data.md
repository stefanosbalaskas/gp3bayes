# Prepare Hierarchical Binary Data

Applies explicit binary outcome mapping, explicit two-level condition
coding, optional recorded numeric scaling, and a model-readiness gate.
No variable is silently scaled or recoded.

## Usage

``` r
prepare_hierarchical_binary_data(
  data,
  contract,
  outcome_mapping = NULL,
  condition_levels = NULL,
  condition_coding = c(-0.5, 0.5),
  scale_predictors = character(),
  scale_time = FALSE,
  missing = c("error", "drop")
)
```

## Arguments

- data:

  A data frame containing the columns declared in `contract`.

- contract:

  A binary `gp3bayes_model_contract`.

- outcome_mapping:

  Optional named vector mapping two labelled outcome values to 0 and 1.
  It is required for non-logical, non-0/1 outcomes.

- condition_levels:

  Optional two-value vector listing the condition levels in
  reference-to-focal order.

- condition_coding:

  Two distinct finite numeric values used to encode the declared
  condition. The default is `c(-0.5, 0.5)`.

- scale_predictors:

  Character vector naming declared numeric predictors to centre and
  divide by their sample standard deviation.

- scale_time:

  Whether to centre and scale the declared linear time variable.

- missing:

  Either `"error"` or `"drop"`. Dropping is performed only after this
  explicit argument is selected, and removed row positions are recorded.

## Value

A `gp3bayes_binary_prepared` object containing the analysis data,
contract, readiness audit, transformation registry, fixed-effects
formula, design-matrix columns, and row accounting.

## Details

This function performs deterministic preparation only. It does not fit a
model, create posterior draws, or establish causal or substantive
validity.

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
  condition_col = "condition",
  predictors = c(
    "participant_covariate",
    "trial_covariate"
  ),
  interaction = c(
    "condition",
    "participant_covariate"
  ),
  random_slope = TRUE
)

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment")
)

prepared
#> <gp3bayes_binary_prepared>
#>   Input rows: 96
#>   Analysis rows: 96
#>   Rows removed: 0
#>   Readiness: ready_with_warnings
#>   Fixed matrix columns: (Intercept), condition, participant_covariate, trial_covariate, condition:participant_covariate
#>   Backend: none
#>   Fit performed: FALSE
```

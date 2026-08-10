# Create an Analysis Manifest

Records the declared analysis contract, transformations, estimands,
sensitivity plan, random seed, data fingerprint, package versions, and
optional sampling specification in one backend-independent provenance
object. The manifest stores a data fingerprint rather than a duplicate
copy of the analysis data.

## Usage

``` r
create_analysis_manifest(
  specification = NULL,
  fit = NULL,
  data = NULL,
  estimands = character(),
  sensitivity_plan = NULL,
  seed = NULL,
  label = NULL,
  notes = character()
)
```

## Arguments

- specification:

  Optional approved gp3bayes model specification.

- fit:

  Optional gp3bayes fit. When supplied, the specification and prepared
  data are derived from the fit unless explicitly supplied.

- data:

  Optional analysis data frame. When omitted it is derived from the
  specification or fit where possible.

- estimands:

  Character vector or structured list describing the prespecified
  estimands.

- sensitivity_plan:

  Optional sensitivity-plan object or list.

- seed:

  Optional non-negative integer seed. When omitted and `fit` is
  supplied, the recorded fitting seed is used.

- label:

  Optional human-readable analysis label.

- notes:

  Optional character notes.

## Value

A `gp3bayes_analysis_manifest`.

## Examples

``` r
simulation <- simulate_hierarchical_binary_data(
  n_participants = 8,
  trials_per_participant = 6,
  n_items = 4,
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
specification <- specify_binary_model(prepared, baseline = 0.35)
manifest <- create_analysis_manifest(
  specification = specification,
  estimands = "standardized_probability_contrast",
  seed = 2026
)
manifest
#> <gp3bayes_analysis_manifest>
#>   Version: 0.2
#>   Family: binary
#>   Data: 48 x 8
#>   Data hash: 2bb5872d3fe01b44bf0abc9a162687e9
#>   Frozen: FALSE
```

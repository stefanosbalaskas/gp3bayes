# Specify the restricted hierarchical pupil time-course model

Constructs an inspectable, closed-set Gaussian model specification.
Users cannot supply a raw formula or arbitrary family.

## Usage

``` r
specify_pupil_timecourse_model(
  prepared,
  temporal_structure = c("smooth", "linear"),
  smooth_basis_dimension = 10L,
  condition_trajectory = NULL,
  autocorrelation = c("ar1", "none"),
  participant_trajectory = c("none", "factor_smooth"),
  item_effects = NULL,
  covariates = character(),
  prior_scales = NULL
)
```

## Arguments

- prepared:

  A
  [`prepare_pupil_timecourse()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prepare_pupil_timecourse.md)
  result.

- temporal_structure:

  `"smooth"` or `"linear"`.

- smooth_basis_dimension:

  Basis dimension for approved smooth terms.

- condition_trajectory:

  `NULL` (default) uses a separate trajectory when condition is
  declared; otherwise supply `TRUE` or `FALSE` explicitly.

- autocorrelation:

  `"ar1"` or `"none"`. AR(1) is blocked when the observed
  sampling-interval coefficient of variation exceeds the recorded
  readiness threshold because sample-order AR(1) is not a
  continuous-time irregular-sampling model.

- participant_trajectory:

  `"none"` or the restricted factor-smooth option `"factor_smooth"`.

- item_effects:

  `NULL` (default) includes an item random intercept only when at least
  two item levels are declared; otherwise supply `TRUE` or `FALSE`
  explicitly.

- covariates:

  Character vector of already-declared numeric nuisance covariates in
  the prepared data.

- prior_scales:

  Optional named positive scale values. Required for pixels and
  arbitrary units.

## Value

A `gp3bayes_pupil_model_specification`.

## Priors

Defaults are unit-aware weak regularizers for physical
millimetres/metres and declared transformed scales. Pixel and
arbitrary-unit outcomes require user-declared prior scales because
tracker-specific units are not interchangeable.

## Governance boundary

No unrestricted formula, likelihood family, smooth, autocorrelation
order, or backend argument is accepted.

## Examples

``` r
sim <- simulate_pupil_timecourse(
  n_participants = 3, trials_per_participant = 3,
  sampling_frequency = 20, seed = 2
)
contract <- create_pupil_contract(
  "pupil_mm", "participant_id", "trial_id", "event_time",
  "millimetres", 20, condition_col = "condition"
)
prepared <- prepare_pupil_timecourse(sim$data, contract)
specify_pupil_timecourse_model(prepared, autocorrelation = "none")
#> <gp3bayes_pupil_model_specification>
#>   Family: Gaussian pupil time-course
#>   Formula: .pupil_model ~ .condition + s(.event_time, by = .condition, k = 10) +      (1 | .participant)
#>   Temporal structure: smooth
#>   Condition trajectory: TRUE
#>   Autocorrelation: none
#>   Outcome unit: millimetres
#>   Baseline: none
#>   Unrestricted formula: FALSE
#>   Fit performed: FALSE
```

# Prepare a pupil time course under explicit transformations

Validates ordering, identifiers, event time, pupil values, measurement
flags, sampling intervals, and baseline support. Only explicitly
requested deterministic transformations are applied and recorded.

## Usage

``` r
prepare_pupil_timecourse(
  data,
  contract,
  baseline_operation = c("none", "subtract", "divide", "proportion_change",
    "percent_change"),
  baseline_window = NULL,
  output_unit = NULL,
  scale_covariates = character(),
  max_rows = 2000000L,
  irregularity_review_cv = 0.1
)
```

## Arguments

- data:

  Source data frame.

- contract:

  A
  [`create_pupil_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_pupil_contract.md)
  result.

- baseline_operation:

  One of `"none"`, `"subtract"`, `"divide"`, `"proportion_change"`, or
  `"percent_change"`.

- baseline_window:

  Required when `baseline_operation != "none"` unless already recorded
  in the contract. Values use the contract's source `time_unit` and are
  converted to canonical seconds during preparation.

- output_unit:

  Optional physical output unit. Only metre/millimetre conversions are
  defined.

- scale_covariates:

  Declared numeric covariates to standardize.

- max_rows:

  Maximum accepted input rows.

- irregularity_review_cv:

  Coefficient-of-variation threshold recorded as a sampling-irregularity
  review signal.

## Value

A `gp3bayes_pupil_prepared` object. The source pupil values are retained
in `.pupil_source`; the modelled values are `.pupil_model`.

## Governance boundary

This function does not detect or interpolate blinks, smooth traces,
choose a baseline, correct PFE, correct luminance, or automatically
exclude samples.

## Examples

``` r
sim <- simulate_pupil_timecourse(
  n_participants = 3, trials_per_participant = 3,
  sampling_frequency = 20, seed = 11
)
contract <- create_pupil_contract(
  "pupil_mm", "participant_id", "trial_id", "event_time",
  "millimetres", 20, condition_col = "condition",
  blink_col = "blink", interpolation_col = "interpolated",
  validity_col = "valid", gaze_x_col = "gaze_x", gaze_y_col = "gaze_y",
  luminance_col = "luminance", baseline_window = c(-0.5, 0)
)
prepared <- prepare_pupil_timecourse(
  sim$data, contract, baseline_operation = "subtract"
)
prepared
#> <gp3bayes_pupil_prepared>
#>   Rows: 549
#>   Participants: 3
#>   Trials: 9
#>   Model unit: millimetres
#>   Baseline operation: subtract
#>   Sampling interval CV: 2.759e-15
#>   Readiness status: review
```

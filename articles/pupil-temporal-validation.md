# Validation for temporally dependent pupil data

## Start with the prediction target

Temporally dependent samples should not automatically be treated as
exchangeable observation-level units.
[`create_pupil_validation_plan()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_pupil_validation_plan.md)
makes the intended target explicit before computing validation.

``` r

sim <- simulate_pupil_timecourse(
  n_participants = 6,
  trials_per_participant = 4,
  sampling_frequency = 10,
  time_window = c(-0.3, 1),
  baseline_window = c(-0.3, 0),
  seed = 21
)
contract <- create_pupil_contract(
  outcome_col = "pupil_mm",
  participant_col = "participant_id",
  trial_col = "trial_id",
  condition_col = "condition",
  time_col = "event_time",
  pupil_unit = "millimetres",
  sampling_frequency = 10
)
prepared <- prepare_pupil_timecourse(sim$data, contract)

trial_plan <- create_pupil_validation_plan(
  prepared,
  target = "new_trial_known_participant",
  K = 4,
  seed = 21
)
trial_plan
#> <gp3bayes_pupil_validation_plan>
#>   Target: new_trial_known_participant
#>   Strategy: grouped_trial_kfold_within_participant
#>   Model-support rows: 330
#>   Leakage detected: FALSE
#>   Automatic strategy selection: FALSE
```

Other supported targets distinguish a new participant and a future time
segment.

``` r

participant_plan <- create_pupil_validation_plan(
  prepared,
  target = "new_participant",
  K = 3,
  seed = 21
)
future_plan <- create_pupil_validation_plan(
  prepared,
  target = "future_segment",
  future_fraction = 0.2
)
participant_plan
#> <gp3bayes_pupil_validation_plan>
#>   Target: new_participant
#>   Strategy: grouped_participant_kfold
#>   Model-support rows: 330
#>   Leakage detected: FALSE
#>   Automatic strategy selection: FALSE
future_plan
#> <gp3bayes_pupil_validation_plan>
#>   Target: future_segment
#>   Strategy: leave_future_segment_out
#>   Model-support rows: 330
#>   Leakage detected: FALSE
#>   Automatic strategy selection: FALSE
```

## Execution

``` r

trial_cv <- validate_pupil_model(
  pupil_fit,
  trial_plan,
  execute = TRUE
)
pupil_validation_table(trial_cv)
plot_pupil_validation(trial_cv)
```

Grouped K-fold uses complete validation groups. The future-segment
target uses a chronological holdout and explicit refitting rather than
presenting ordinary observation-wise PSIS-LOO as a universal time-series
solution. Validation answers only the declared predictive question.

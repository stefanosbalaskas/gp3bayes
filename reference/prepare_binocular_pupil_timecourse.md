# Prepare binocular pupil data without averaging eyes

Prepare binocular pupil data without averaging eyes

## Usage

``` r
prepare_binocular_pupil_timecourse(
  data,
  left_col = "pupil_left",
  right_col = "pupil_right",
  participant_col = "participant_id",
  time_col = "time_ms",
  condition_col = "condition",
  trial_col = "trial_id",
  item_col = NULL,
  covariates = character()
)
```

## Arguments

- data:

  A data frame containing left and right pupil responses.

- left_col, right_col:

  Left/right pupil response columns.

- participant_col, time_col, condition_col:

  Structural columns.

- trial_col, item_col:

  Optional structural columns.

- covariates:

  Optional additional covariates.

## Value

A `gp3bayes_binocular_pupil_prepared` object.

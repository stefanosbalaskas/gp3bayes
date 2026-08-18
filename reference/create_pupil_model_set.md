# Create a named set of fitted pupil models

Create a named set of fitted pupil models

## Usage

``` r
create_pupil_model_set(
  ...,
  predictive_target = c("new_trial_known_participant", "new_participant",
    "future_segment", "new_sample_known_trial")
)
```

## Arguments

- ...:

  Fitted advanced or compatible brms-backed pupil models, or one named
  list of models.

- predictive_target:

  Declared target for interpreting comparison.

## Value

A `gp3bayes_pupil_model_set` object.

# Audit posterior predictive calibration on explicit evaluation data

Audit posterior predictive calibration on explicit evaluation data

## Usage

``` r
audit_pupil_predictive_calibration(
  fit,
  newdata,
  ndraws = 500L,
  probability = 0.9,
  population_only = FALSE,
  allow_new_levels = FALSE
)
```

## Arguments

- fit:

  An advanced fitted model.

- newdata:

  Evaluation data containing the pupil response.

- ndraws:

  Number of posterior predictive draws.

- probability:

  Interval probability.

- population_only:

  Exclude group-level effects if TRUE.

- allow_new_levels:

  Passed to brms prediction.

## Value

A predictive-score object with evaluation metadata.

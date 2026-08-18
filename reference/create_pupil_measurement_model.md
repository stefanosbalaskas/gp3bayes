# Create a governed measurement-uncertainty specification

Declares known standard-error columns for pupil covariates and/or the
pupil response. The object records uncertainty; it does not alter or
impute data.

## Usage

``` r
create_pupil_measurement_model(
  baseline_error = NULL,
  luminance_error = NULL,
  gaze_error = NULL,
  response_error = NULL,
  covariate_errors = NULL
)
```

## Arguments

- baseline_error, luminance_error, gaze_error:

  Optional standard-error column names for common adjustment variables.

- response_error:

  Optional known standard-error column for the pupil response.

- covariate_errors:

  Optional named character vector mapping arbitrary covariate names to
  standard-error columns.

## Value

A `gp3bayes_pupil_measurement_model` object.

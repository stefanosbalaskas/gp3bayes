# Create a governed pupil sensitivity suite

Declares scientifically consequential alternatives without choosing the
alternative that produces the largest effect. The suite is inert until a
scenario is explicitly materialized or results are supplied for
comparison.

## Usage

``` r
create_pupil_sensitivity_suite(
  specification,
  baseline_windows = list(),
  baseline_window_operation = NULL,
  baseline_operations = character(),
  interpolation_policy = character(),
  blink_adjacent_margins = numeric(),
  gaze_adjustment = character(),
  luminance_adjustment = character(),
  pfe_prepared = list(),
  smooth_basis_dimensions = integer(),
  autocorrelation = character(),
  analysis_windows = list()
)
```

## Arguments

- specification:

  Baseline pupil model specification.

- baseline_windows:

  List of alternative two-element baseline windows.

- baseline_window_operation:

  Optional baseline transformation to pair with `baseline_windows` when
  the baseline specification used `"none"`. This must be declared
  explicitly; gp3bayes never chooses one.

- baseline_operations:

  Alternative baseline transformations.

- interpolation_policy:

  `"retain"` and/or `"exclude_flagged"`.

- blink_adjacent_margins:

  Non-negative margins in seconds; zero means no blink-adjacent
  deletion.

- gaze_adjustment:

  `"none"` and/or `"declared_covariates"`.

- luminance_adjustment:

  `"none"` and/or `"declared_covariate"`.

- pfe_prepared:

  Optional named list of explicitly prepared alternative pupil series
  (for example upstream corrected and uncorrected versions). gp3bayes
  does not perform PFE correction.

- smooth_basis_dimensions:

  Alternative approved basis dimensions.

- autocorrelation:

  Alternative `"none"`/`"ar1"` structures.

- analysis_windows:

  List of declared estimand windows in canonical event-time seconds.

## Value

A `gp3bayes_pupil_sensitivity` plan.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

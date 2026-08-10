# Create a Sensitivity Suite Plan

Creates a declarative plan for sensitivity analyses already supported by
gp3bayes. Expensive refits are opt-in and are never started merely by
creating a plan.

## Usage

``` r
create_sensitivity_suite_plan(
  prior_scale = FALSE,
  powerscale = FALSE,
  psis_loo = FALSE,
  random_slope_plan = NULL,
  group_deletion_plan = NULL,
  alternative_estimands = list(),
  duration_unit = NULL,
  prior_scale_args = list(),
  powerscale_args = list(),
  psis_args = list(),
  random_slope_args = list(),
  group_deletion_args = list()
)
```

## Arguments

- prior_scale:

  Whether to run the family-specific prior-scale refit.

- powerscale:

  Whether to run local power-scaling through `priorsense`.

- psis_loo:

  Whether to compute PSIS-LOO for the reference fit.

- random_slope_plan:

  Optional result of
  [`create_random_slope_sensitivity_plan()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_random_slope_sensitivity_plan.md).

- group_deletion_plan:

  Optional result of
  [`create_group_deletion_sensitivity_plan()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_group_deletion_sensitivity_plan.md).

- alternative_estimands:

  Optional named list of already-computed estimands from
  coding/scaling/unit or other approved sensitivity fits.

- duration_unit:

  Optional list containing `estimand`, `multiplier`, and optional
  `tolerance` for
  [`audit_duration_unit_invariance()`](https://stefanosbalaskas.github.io/gp3bayes/reference/audit_duration_unit_invariance.md).

- prior_scale_args:

  Named argument list passed to the family-specific prior-scale
  sensitivity function.

- powerscale_args:

  Named argument list passed to
  [`assess_powerscaled_sensitivity()`](https://stefanosbalaskas.github.io/gp3bayes/reference/assess_powerscaled_sensitivity.md).

- psis_args:

  Named argument list passed to
  [`compute_psis_loo()`](https://stefanosbalaskas.github.io/gp3bayes/reference/compute_psis_loo.md).

- random_slope_args:

  Named argument list passed to
  [`run_random_slope_sensitivity()`](https://stefanosbalaskas.github.io/gp3bayes/reference/run_random_slope_sensitivity.md).

- group_deletion_args:

  Named argument list passed to
  [`run_group_deletion_sensitivity()`](https://stefanosbalaskas.github.io/gp3bayes/reference/run_group_deletion_sensitivity.md).

## Value

A `gp3bayes_sensitivity_plan`.

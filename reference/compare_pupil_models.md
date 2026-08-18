# Compare fitted pupil models predictively

Compare fitted pupil models predictively

## Usage

``` r
compare_pupil_models(
  model_set,
  criterion = c("loo", "kfold"),
  K = 10L,
  group = NULL,
  moment_match = FALSE,
  save_psis = TRUE
)
```

## Arguments

- model_set:

  A named model set.

- criterion:

  `"loo"` or exact `"kfold"`. Leave-future-out is handled by explicit
  plans via
  [`create_pupil_lfo_plan()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_pupil_lfo_plan.md)
  and
  [`validate_pupil_leave_future_out()`](https://stefanosbalaskas.github.io/gp3bayes/reference/validate_pupil_leave_future_out.md).

- K:

  Number of folds for exact K-fold CV.

- group:

  Optional grouping column passed to brms K-fold.

- moment_match:

  Use brms/loo moment matching for PSIS-LOO where supported.

- save_psis:

  Save PSIS objects.

## Value

A `gp3bayes_pupil_model_comparison` object.

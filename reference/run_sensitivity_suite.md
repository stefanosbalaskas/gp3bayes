# Run a Unified Sensitivity Suite

Orchestrates approved sensitivity components. Refitting components only
run when explicitly requested in `plan`. Failures are retained as
inspectable component results unless `stop_on_error = TRUE`.

## Usage

``` r
run_sensitivity_suite(
  fit,
  plan = create_sensitivity_suite_plan(),
  reference_estimand = NULL,
  stop_on_error = FALSE
)
```

## Arguments

- fit:

  An approved gp3bayes fit.

- plan:

  A
  [`create_sensitivity_suite_plan()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_sensitivity_suite_plan.md)
  result.

- reference_estimand:

  Optional precomputed primary estimand. When alternatives are supplied
  and this is omitted it is computed with
  [`estimate_model_estimands()`](https://stefanosbalaskas.github.io/gp3bayes/reference/estimate_model_estimands.md).

- stop_on_error:

  Whether the first component error should stop the suite.

## Value

A `gp3bayes_sensitivity_suite`.

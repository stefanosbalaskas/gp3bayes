# Create a Structured Duration Model Report

Writes a conservative Markdown report for an approved fitted lognormal
duration model.

## Usage

``` r
create_duration_model_report(
  fit,
  diagnostics = NULL,
  posterior_summary = NULL,
  posterior_predictive = NULL,
  prior_sensitivity = NULL,
  recovery = NULL,
  file = "gp3bayes-duration-model-report.md",
  overwrite = FALSE
)
```

## Arguments

- fit:

  A `gp3bayes_duration_fit`.

- diagnostics:

  Optional result from
  [`diagnose_duration_fit()`](https://stefanosbalaskas.github.io/gp3bayes/reference/diagnose_duration_fit.md).

- posterior_summary:

  Optional result from
  [`summarise_duration_posterior()`](https://stefanosbalaskas.github.io/gp3bayes/reference/summarise_duration_posterior.md).

- posterior_predictive:

  Optional result from
  [`check_duration_posterior_predictive()`](https://stefanosbalaskas.github.io/gp3bayes/reference/check_duration_posterior_predictive.md).

- prior_sensitivity:

  Optional result from
  [`assess_duration_prior_sensitivity()`](https://stefanosbalaskas.github.io/gp3bayes/reference/assess_duration_prior_sensitivity.md).

- recovery:

  Optional result from
  [`run_duration_recovery()`](https://stefanosbalaskas.github.io/gp3bayes/reference/run_duration_recovery.md).

- file:

  Output Markdown path.

- overwrite:

  Whether an existing file may be replaced.

## Value

A `gp3bayes_duration_model_report`.

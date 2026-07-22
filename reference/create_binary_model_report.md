# Create a Structured Binary Model Report

Writes a conservative Markdown report for an approved fitted binary
model.

## Usage

``` r
create_binary_model_report(
  fit,
  diagnostics = NULL,
  posterior_summary = NULL,
  posterior_predictive = NULL,
  prior_sensitivity = NULL,
  recovery = NULL,
  file = "gp3bayes-binary-model-report.md",
  overwrite = FALSE
)
```

## Arguments

- fit:

  A `gp3bayes_binary_fit`.

- diagnostics:

  Optional result from
  [`diagnose_binary_fit()`](https://stefanosbalaskas.github.io/gp3bayes/reference/diagnose_binary_fit.md).

- posterior_summary:

  Optional result from
  [`summarise_binary_posterior()`](https://stefanosbalaskas.github.io/gp3bayes/reference/summarise_binary_posterior.md).

- posterior_predictive:

  Optional result from
  [`check_binary_posterior_predictive()`](https://stefanosbalaskas.github.io/gp3bayes/reference/check_binary_posterior_predictive.md).

- prior_sensitivity:

  Optional result from
  [`assess_binary_prior_sensitivity()`](https://stefanosbalaskas.github.io/gp3bayes/reference/assess_binary_prior_sensitivity.md).

- recovery:

  Optional result from
  [`run_binary_recovery()`](https://stefanosbalaskas.github.io/gp3bayes/reference/run_binary_recovery.md).

- file:

  Output Markdown file.

- overwrite:

  Whether an existing file may be replaced.

## Value

A `gp3bayes_binary_model_report` containing the normalized path and
section-status registry.

## Details

The report never converts diagnostic or predictive statuses into an
automatic statement that the model converged or is substantively valid.

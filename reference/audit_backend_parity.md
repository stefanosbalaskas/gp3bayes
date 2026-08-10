# Audit Posterior Parity Across rstan and cmdstanr

Compares posterior summaries from two independently sampled fits. Mean
differences are evaluated relative to the combined Monte Carlo standard
error rather than requiring identical draws. Standard-deviation
differences are reported separately. A parity pass is a computational
consistency check, not evidence that either model is statistically or
substantively adequate.

## Usage

``` r
audit_backend_parity(
  rstan_fit,
  cmdstanr_fit,
  variables = NULL,
  mcse_multiplier = 3,
  absolute_tolerance = 0,
  relative_sd_tolerance = 0.1
)
```

## Arguments

- rstan_fit:

  A gp3bayes rstan fit, or a compatible posterior-summary data frame for
  testing/auditing.

- cmdstanr_fit:

  A gp3bayes cmdstanr fit, or a compatible summary table.

- variables:

  Optional parameter names to compare. When omitted, the package's
  approved population/group-scale parameter set is used.

- mcse_multiplier:

  Multiplier applied to the combined MCSE of posterior means to define a
  sampling-noise comparison band.

- absolute_tolerance:

  Minimum absolute tolerance for mean differences.

- relative_sd_tolerance:

  Review threshold for relative posterior-SD differences.

## Value

A `gp3bayes_backend_parity_audit`.

## Examples

``` r
rstan_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.6, 0.4), sd = c(0.20, 0.15),
  mcse_mean = c(0.01, 0.01)
)
cmdstanr_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.59, 0.41), sd = c(0.21, 0.15),
  mcse_mean = c(0.01, 0.01)
)
audit_backend_parity(rstan_summary, cmdstanr_summary)
#> <gp3bayes_backend_parity_audit>
#>   Status: pass
#>   Parameters compared: 2
#>   Review parameters: 0
#>   Identical draws expected: FALSE
```

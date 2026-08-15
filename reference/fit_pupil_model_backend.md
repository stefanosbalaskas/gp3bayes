# Fit the restricted pupil time-course model with an approved backend

Fits only a
[`specify_pupil_timecourse_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/specify_pupil_timecourse_model.md)
specification through `brms` using full MCMC sampling and either `rstan`
or `cmdstanr`.

## Usage

``` r
fit_pupil_model_backend(
  specification,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = min(2L, chains),
  seed = 2026,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)
```

## Arguments

- specification:

  Approved pupil model specification.

- backend:

  `"rstan"` or `"cmdstanr"`.

- chains, iter, warmup, cores, seed:

  Sampling controls. Package-controlled cores are capped at two.

- adapt_delta, max_treedepth, refresh:

  Fixed safe sampling controls.

## Value

A `gp3bayes_pupil_fit`.

## Governance boundary

This interface accepts no arbitrary formula, family, Stan program,
inference algorithm, or unrestricted backend arguments. A returned fit
does not establish convergence, adequacy, predictive validity, or
psychological interpretation.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

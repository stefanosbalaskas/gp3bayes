# Assess Duration Prior Sensitivity

Refits the approved duration model under prespecified tighter and wider
prior scales and compares population-level and residual posterior
medians.

## Usage

``` r
assess_duration_prior_sensitivity(
  fit,
  scale_multipliers = c(tighter = 0.5, wider = 2),
  chains = fit$sampling$chains,
  iter = fit$sampling$iter,
  warmup = fit$sampling$warmup,
  cores = fit$sampling$cores,
  seed = fit$sampling$seed + 2000L,
  adapt_delta = fit$sampling$adapt_delta,
  max_treedepth = fit$sampling$max_treedepth,
  refresh = 0L,
  maximum_standardized_shift = 0.25,
  review_standardized_shift = 0.5,
  retain_fits = FALSE
)
```

## Arguments

- fit:

  A `gp3bayes_duration_fit`.

- scale_multipliers:

  Named positive prior-scale multipliers.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted sampling controls passed to
  [`fit_duration_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_duration_model.md).

- maximum_standardized_shift:

  Maximum standardized median shift for pass.

- review_standardized_shift:

  Maximum standardized median shift for review.

- retain_fits:

  Whether alternative fits are retained.

## Value

A `gp3bayes_duration_prior_sensitivity`.

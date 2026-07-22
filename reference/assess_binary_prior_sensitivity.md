# Assess Binary Prior Sensitivity

Refits the approved binary model under prespecified tighter and wider
prior scales and compares population-level posterior medians.

## Usage

``` r
assess_binary_prior_sensitivity(
  fit,
  scale_multipliers = c(tighter = 0.5, wider = 2),
  chains = fit$sampling$chains,
  iter = fit$sampling$iter,
  warmup = fit$sampling$warmup,
  cores = fit$sampling$cores,
  seed = fit$sampling$seed + 1000L,
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

  A `gp3bayes_binary_fit`.

- scale_multipliers:

  Named positive numeric multipliers applied to the intercept,
  coefficient, and group-scale priors.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted sampling controls passed to
  [`fit_binary_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model.md).

- maximum_standardized_shift:

  Maximum absolute posterior-median shift, divided by the reference
  posterior standard deviation, for a pass.

- review_standardized_shift:

  Maximum standardized shift for review.

- retain_fits:

  Whether alternative fitted objects should be retained.

## Value

A `gp3bayes_binary_prior_sensitivity` object.

## Details

This function is computationally expensive. Sensitivity status describes
stability under the declared scale changes only; it does not prove prior
robustness under all defensible priors.

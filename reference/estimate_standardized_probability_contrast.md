# Estimate a Design-Standardised Binary Probability Contrast

Replaces the focal-condition value across a declared target covariate
distribution, obtains population-level expected probabilities using
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html),
and averages within each posterior draw.

## Usage

``` r
estimate_standardized_probability_contrast(
  fit,
  target_data = NULL,
  target_scale = c("prepared", "raw"),
  ndraws = NULL,
  include_group_effects = FALSE
)
```

## Arguments

- fit:

  An approved gp3bayes binary fit.

- target_data:

  Optional target covariate distribution.

- target_scale:

  Whether supplied target data are raw or already prepared.

- ndraws:

  Optional number of posterior draws.

- include_group_effects:

  Whether recorded group-level effects are included. The default `FALSE`
  targets population-level predictions.

## Value

A `gp3bayes_estimand` with probability-difference draws.

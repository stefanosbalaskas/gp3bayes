# Create a Prediction Contrast Profile

Create a Prediction Contrast Profile

## Usage

``` r
create_prediction_contrast_profile(
  fit,
  variable,
  contrast_variable,
  contrast_levels = NULL,
  values = NULL,
  n = 40L,
  at = list(),
  measure = c("difference", "ratio", "odds_ratio"),
  include_group_effects = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variable:

  Numeric profile variable.

- contrast_variable:

  Variable defining two contrasted levels.

- contrast_levels:

  Optional two levels.

- values:

  Optional profile values.

- n:

  Number of values when `values` is omitted.

- at:

  Named values for other predictors.

- measure:

  `"difference"`, `"ratio"`, or `"odds_ratio"`.

- include_group_effects:

  Whether group effects are included.

- ndraws:

  Optional posterior draws.

- probs:

  Three interval probabilities.

## Value

A `gp3bayes_prediction_contrast_profile`.

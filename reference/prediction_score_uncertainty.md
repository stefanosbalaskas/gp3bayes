# Posterior Uncertainty in Prediction Scores

Binary fits use Brier and logarithmic scores; duration fits use RMSE and
MAE.

## Usage

``` r
prediction_score_uncertainty(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- newdata:

  Optional data containing observed outcomes.

- include_group_effects:

  Whether fitted group effects are included.

- ndraws:

  Expected-response posterior draws.

- probs:

  Three interval probabilities.

## Value

A `gp3bayes_prediction_score_uncertainty`.

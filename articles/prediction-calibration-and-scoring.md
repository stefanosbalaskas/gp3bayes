# Prediction, Calibration, and Scoring

The prediction API distinguishes conditional expected responses from new
posterior predictive outcomes. This distinction is retained in the
returned `gp3bayes_prediction` object and in downstream calibration and
scoring tools.

``` r

library(gp3bayes)

binary_prediction_scores(
  c(0.05, 0.20, 0.75, 0.90),
  c(0, 0, 1, 1)
)
#>   n   brier  log_loss auc threshold accuracy sensitivity specificity
#> 1 4 0.02875 0.1668699   1       0.5        1           1           1
#>   balanced_accuracy automatic_decision
#> 1                 1              FALSE

binary_threshold_metrics(
  c(0.05, 0.20, 0.75, 0.90),
  c(0, 0, 1, 1),
  thresholds = c(0.3, 0.5, 0.7)
)
#>   n   brier  log_loss auc threshold accuracy sensitivity specificity
#> 1 4 0.02875 0.1668699   1       0.3        1           1           1
#> 2 4 0.02875 0.1668699   1       0.5        1           1           1
#> 3 4 0.02875 0.1668699   1       0.7        1           1           1
#>   balanced_accuracy automatic_decision
#> 1                 1              FALSE
#> 2                 1              FALSE
#> 3                 1              FALSE

duration_prediction_scores(
  c(900, 1100, 1300),
  c(950, 1050, 1400)
)
#>   n      mae     rmse median_absolute_error    log_mae   log_rmse
#> 1 3 66.66667 70.71068                    50 0.05823174 0.05938397
#>   mean_log_error automatic_decision
#> 1    -0.02721839              FALSE
```

## Fitted predictions

``` r

grid <- create_prediction_grid(
  fit,
  at = list(condition = c("control", "treatment"))
)

support <- audit_prediction_support(fit, grid)

expected <- predict_model(
  fit,
  newdata = grid,
  type = "expected",
  include_group_effects = FALSE
)

predictive <- predict_model(
  fit,
  newdata = grid,
  type = "predictive",
  include_group_effects = FALSE,
  ndraws = 1000
)

prediction_table(expected)
plot_prediction_intervals(expected)
plot_prediction_support(support)
```

For binary fits, expected predictions are event probabilities and can be
used for calibration and threshold summaries. For duration fits, the API
separately exposes the arithmetic expected response, conditional median,
and new-outcome posterior predictive distribution.

``` r

p_binary <- predict_binary_probability(binary_fit)
calibration <- binary_calibration_table(p_binary)
plot_binary_calibration(calibration)

p_duration <- predict_duration(duration_fit, type = "predictive")
duration_quantile_calibration(p_duration)
duration_pit_table(p_duration)
predictive_coverage_table(p_duration)
```

All reported metrics are descriptive. The package does not choose a
threshold or certify predictive adequacy automatically.

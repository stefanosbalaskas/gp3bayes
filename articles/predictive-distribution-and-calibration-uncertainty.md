# Predictive Distribution and Calibration Uncertainty

Posterior predictive diagnostics can retain uncertainty in entire
outcome distributions as well as in predictive scores.

``` r

atlas <- create_predictive_distribution_atlas(
  fit,
  ndraws = 500,
  include_group_effects = TRUE,
  seed = 2026
)

plot_predictive_atlas_statistics(atlas)

quantiles <- predictive_quantile_envelope(atlas)
plot_predictive_quantile_envelope(quantiles)

scores <- prediction_score_uncertainty(
  fit,
  ndraws = 1000
)

prediction_score_uncertainty_table(scores)
plot_prediction_score_uncertainty(scores)
```

For binary models:

``` r

calibration <- binary_calibration_uncertainty(
  fit,
  bins = 10,
  ndraws = 1000
)

binary_calibration_uncertainty_table(calibration)
plot_binary_calibration_uncertainty(calibration)
```

These summaries do not automatically establish calibration, predictive
adequacy, or model superiority.

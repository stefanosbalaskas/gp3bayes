# Prediction Profiles, Surfaces, and Contrast Profiles

Governed prediction grids can be explored as one-dimensional profiles,
finite-difference predictive gradients, two-dimensional surfaces, and
two-level contrast profiles.

``` r

profile <- create_prediction_profile(
  fit,
  variable = "trial_index",
  type = "expected"
)

prediction_gradient_table(profile)
plot_prediction_profile(profile)
plot_prediction_gradient(profile)

surface <- create_prediction_surface(
  fit,
  x = "trial_index",
  y = "standardized_covariate"
)

plot_prediction_surface(surface)
plot_prediction_surface_uncertainty(surface)

contrast <- create_prediction_contrast_profile(
  fit,
  variable = "trial_index",
  contrast_variable = "condition",
  contrast_levels = c("control", "treatment"),
  measure = "difference"
)

plot_prediction_contrast_profile(contrast)
```

These are fitted predictive descriptions, not causal response curves or
automatic interaction tests.

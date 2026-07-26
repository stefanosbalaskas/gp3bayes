# First-Class Estimands and Sensitivity Workflows

## Why estimands are first-class

gp3bayes distinguishes model coefficients from substantive quantities.
Binary workflows can report a design-standardised probability contrast.
Duration workflows can report conditional-median differences and ratios
and a declared posterior predictive upper quantile. None is
automatically interpreted as a causal effect.

## Binary probability standardisation

The fitting code below is not executed while building the article.

``` r

fit_binary <- fit_binary_model_backend(
  binary_specification,
  backend = "cmdstanr"
)

binary_estimand <- estimate_standardized_probability_contrast(
  fit_binary,
  target_data = target_population,
  target_scale = "raw",
  include_group_effects = FALSE
)

summarise_estimand_draws(binary_estimand)
plot(binary_estimand, quantity = "probability_difference")
```

The target rows define the covariate distribution over which expected
probabilities are averaged. With `include_group_effects = FALSE`,
predictions are population-level rather than conditioned on observed
group effects.

## Duration median and predictive-tail estimands

``` r

fit_duration <- fit_duration_model_backend(
  duration_specification,
  backend = "cmdstanr"
)

duration_estimand <- estimate_standardized_duration_estimands(
  fit_duration,
  predictive_quantile = 0.90,
  include_group_effects = FALSE,
  seed = 2026
)

summarise_estimand_draws(duration_estimand)
plot(duration_estimand, quantity = "conditional_median_ratio")
plot(duration_estimand, quantity = "predictive_quantile_difference")
```

The exponentiated lognormal location contrast is treated as a
conditional median ratio, not an arithmetic-mean ratio. Predictive
quantiles include residual predictive variation.

## Structural sensitivity

``` r

random_slope_plan <- create_random_slope_sensitivity_plan(
  binary_specification
)

random_slope_result <- run_random_slope_sensitivity(
  random_slope_plan,
  backend = "cmdstanr"
)

random_slope_result$comparison
```

No structure is selected automatically. The workflow asks whether the
declared estimand materially changes under the approved random-slope
alternative.

## Participant and item deletion

``` r

participant_plan <- create_group_deletion_sensitivity_plan(
  binary_specification,
  group = "participant",
  units = c("p001", "p002", "p003")
)

participant_result <- run_group_deletion_sensitivity(
  participant_plan,
  backend = "cmdstanr"
)
```

Omission is a sensitivity analysis, not an exclusion rule. For designs
with many groups, units must be supplied explicitly rather than
launching an unbounded sequence of refits.

## Parameterisation sensitivity

``` r

contrast_spec <- create_contrast_coding_sensitivity_specification(
  binary_specification,
  condition_coding = c(0, 1),
  baseline = 0.35
)

scaling_spec <- create_predictor_scaling_sensitivity_specification(
  binary_specification,
  predictor = "trial_covariate",
  scale_factor = 2,
  coefficient_scale = 1.50,
  interaction_scale = 0.50
)

seconds_spec <- create_duration_unit_sensitivity_specification(
  duration_specification,
  multiplier = 0.001,
  new_unit = "seconds"
)
```

Alternative codings and scales require explicit prior choices where
prior meaning changes. Unit conversion is handled separately because
ratios should be unit-free while absolute duration quantities must scale
by the declared factor.

## Exact K-fold validation

``` r

kfold <- compute_kfold_cv(
  fit_binary,
  K = 5,
  folds = "grouped",
  group = "participant_id"
)
kfold
```

Exact K-fold is deliberately an optional, expensive
predictive-validation adapter. It complements PSIS-LOO when refitting is
scientifically appropriate; it never becomes an automatic best-model
selector.

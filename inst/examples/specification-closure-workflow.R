# gp3bayes specification-closure example
#
# This script exercises backend-independent closure workflows. Expensive model
# fitting, posterior predictive checking, and K-fold refitting are shown in the
# package vignettes and optional real-backend smoke script.

library(gp3bayes)

binary_sim <- simulate_hierarchical_binary_data(
  n_participants = 16L,
  trials_per_participant = 8L,
  n_items = 8L,
  seed = 2026L
)

binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = c("participant_covariate", "trial_covariate"),
  interaction = c("condition", "participant_covariate"),
  random_slope = FALSE
)

strict <- audit_model_readiness_strict(
  binary_sim$data,
  binary_contract,
  run_separation = FALSE
)
print(strict)

prepared <- prepare_hierarchical_binary_data(
  binary_sim$data,
  binary_contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = c("participant_covariate", "trial_covariate")
)

recipe <- create_transformation_recipe(prepared)
print(recipe)
print(validate_transformation_replay(prepared))

specification <- specify_binary_model_with_interaction_prior(
  prepared,
  baseline = 0.35
)

print(create_random_slope_sensitivity_plan(specification))
print(create_group_deletion_sensitivity_plan(
  specification,
  group = "participant",
  units = unique(as.character(prepared$data$participant_id))[1:2]
))

print(gp3bayes_specification_traceability())

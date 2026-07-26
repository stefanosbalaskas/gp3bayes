options(warn = 2)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required for the specification-closure smoke test.")
}

pkgload::load_all(".", reset = TRUE, export_all = FALSE, helpers = FALSE, quiet = TRUE)

bin <- simulate_hierarchical_binary_data(
  n_participants = 12L,
  trials_per_participant = 8L,
  n_items = 6L,
  seed = 5501L
)

bin_contract <- create_model_contract(
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

balance <- summarise_condition_balance(bin$data, bin_contract)
stopifnot(inherits(balance, "gp3bayes_condition_balance"))

variation <- summarise_binary_group_variation(bin$data, bin_contract)
stopifnot(inherits(variation, "gp3bayes_binary_group_variation"))

strict <- audit_model_readiness_strict(
  bin$data,
  bin_contract,
  run_separation = FALSE
)
stopifnot(
  inherits(strict, "gp3bayes_strict_readiness_audit"),
  all(c(
    "overall_condition_balance",
    "participant_binary_outcome_variation",
    "identifier_like_predictors",
    "fixed_effect_rank"
  ) %in% strict$checks$check_id)
)

prepared <- prepare_hierarchical_binary_data(
  bin$data,
  bin_contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = c("participant_covariate", "trial_covariate")
)
replay <- validate_transformation_replay(prepared)
stopifnot(inherits(replay, "gp3bayes_transformation_replay_audit"), replay$status == "pass")

spec <- specify_binary_model_with_interaction_prior(prepared, baseline = 0.35)
plan <- create_random_slope_sensitivity_plan(spec)
stopifnot(
  inherits(plan, "gp3bayes_random_slope_sensitivity_plan"),
  identical(plan$automatic_selection, FALSE)
)

dur <- simulate_hierarchical_duration_data(
  n_participants = 12L,
  trials_per_participant = 8L,
  n_items = 6L,
  outcome_unit = "milliseconds",
  seed = 5502L
)
dur_contract <- create_model_contract(
  family = "duration",
  outcome_col = "duration",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = c("participant_covariate", "trial_covariate"),
  interaction = c("condition", "participant_covariate"),
  outcome_unit = "milliseconds"
)
extremes <- review_duration_extremes(dur$data, dur_contract)
bounds <- audit_duration_boundaries(
  dur$data,
  dur_contract,
  allowed_range = c(1, max(dur$data$duration) * 2)
)
stopifnot(
  inherits(extremes, "gp3bayes_duration_extreme_review"),
  inherits(bounds, "gp3bayes_duration_boundary_audit"),
  bounds$status %in% c("pass", "review")
)

trace <- gp3bayes_specification_traceability()
stopifnot(nrow(trace) >= 18L, all(trace$status == "implemented"))

cat("SPECIFICATION CLOSURE SMOKE TEST PASSED\n")

# Backend-independent smoke test for the gp3bayes 0.2.0 stabilization layer.

stopifnot(file.exists("DESCRIPTION"))
devtools::load_all(quiet = TRUE)

simulation <- simulate_hierarchical_binary_data(
  n_participants = 10,
  trials_per_participant = 8,
  n_items = 5,
  random_slope_sd = 0,
  seed = 9020
)
contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = "trial_covariate"
)

design <- audit_design_support(
  simulation$data,
  contract,
  separation = FALSE,
  strict_readiness = TRUE
)
stopifnot(inherits(design, "gp3bayes_design_support_audit"))

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = "trial_covariate"
)
specification <- specify_binary_model(prepared, baseline = 0.35)

manifest <- create_analysis_manifest(
  specification = specification,
  estimands = "standardized_probability_contrast",
  sensitivity_plan = create_sensitivity_suite_plan(prior_scale = TRUE, psis_loo = TRUE),
  seed = 9021,
  label = "0.2.0 stabilization smoke test"
)
frozen <- freeze_analysis_manifest(manifest)
stopifnot(isTRUE(frozen$frozen), nzchar(frozen$manifest_hash))

validation <- validate_gp3bayes_object(specification)
stopifnot(identical(validation$status, "pass"))

schema <- capture_gp3bayes_schema(specification)
stopifnot(identical(validate_gp3bayes_schema(specification, schema)$status, "pass"))

rstan_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.60, 0.40), sd = c(0.20, 0.15), mcse_mean = c(0.01, 0.01)
)
cmdstanr_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.59, 0.41), sd = c(0.21, 0.15), mcse_mean = c(0.01, 0.01)
)
parity <- audit_backend_parity(rstan_summary, cmdstanr_summary)
stopifnot(identical(parity$status, "pass"))

cat("Backend-independent 0.2.0 stabilization smoke test: PASS\n")

# Mirror the backend-independent stabilization surface for duration data.
duration_simulation <- simulate_hierarchical_duration_data(
  n_participants = 10,
  trials_per_participant = 8,
  n_items = 5,
  random_slope_sd = 0,
  seed = 9030
)
duration_contract <- create_model_contract(
  "duration", "duration", "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit = "milliseconds"
)
duration_design <- audit_design_support(
  duration_simulation$data,
  duration_contract,
  separation = FALSE,
  strict_readiness = TRUE
)
duration_prepared <- prepare_hierarchical_duration_data(
  duration_simulation$data,
  duration_contract,
  condition_levels = c("control", "treatment")
)
duration_specification <- specify_duration_model(
  duration_prepared,
  baseline = 500
)
duration_manifest <- freeze_analysis_manifest(
  create_analysis_manifest(
    duration_specification,
    estimands = "standardized_duration_estimands",
    seed = 9031
  )
)
stopifnot(
  inherits(duration_design, "gp3bayes_design_support_audit"),
  identical(duration_design$family, "duration"),
  identical(validate_gp3bayes_object(duration_specification)$status, "pass"),
  isTRUE(duration_manifest$frozen)
)

cat("Backend-independent duration stabilization smoke test: PASS\n")

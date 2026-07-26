# Optional real-backend specification-closure smoke test.
#
# Environment flags:
#   GP3BAYES_CLOSURE_BACKEND=rstan|cmdstanr
#   GP3BAYES_CLOSURE_KFOLD=1  (optional and intentionally expensive)

if (!requireNamespace("pkgload", quietly = TRUE)) stop("pkgload is required.")
pkgload::load_all(".", reset = TRUE, export_all = FALSE, helpers = FALSE, quiet = TRUE)

backend <- Sys.getenv("GP3BAYES_CLOSURE_BACKEND", "cmdstanr")
if (!backend %in% c("rstan", "cmdstanr")) stop("Unsupported closure smoke backend.")
run_kfold <- identical(Sys.getenv("GP3BAYES_CLOSURE_KFOLD", ""), "1")

bin <- simulate_hierarchical_binary_data(
  n_participants = 12L,
  trials_per_participant = 8L,
  n_items = 6L,
  random_slope_sd = 0,
  seed = 5601L
)
contract <- create_model_contract(
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
prepared <- prepare_hierarchical_binary_data(
  bin$data,
  contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = c("participant_covariate", "trial_covariate")
)
spec <- specify_binary_model_with_interaction_prior(prepared, baseline = 0.35)

fit <- fit_binary_model_backend(
  spec,
  backend = backend,
  chains = 2L,
  iter = 600L,
  warmup = 300L,
  cores = 2L,
  seed = 5602L,
  refresh = 0L
)

estimand <- estimate_standardized_probability_contrast(fit, ndraws = 200L)
stopifnot(inherits(estimand, "gp3bayes_estimand"))
summary <- summarise_estimand_draws(estimand, "probability_difference")
stopifnot(nrow(summary) == 1L, is.finite(summary$median))

ppc <- check_binary_ppc_details(
  fit,
  draws = 40L,
  seed = 5603L,
  calibration_bins = 5L
)
stopifnot(inherits(ppc, "gp3bayes_binary_ppc_detail"))

if (run_kfold) {
  cv <- compute_kfold_cv(
    fit,
    K = 2L,
    folds = "grouped",
    group = "participant_id",
    save_fits = FALSE,
    seed = 5604L
  )
  stopifnot(inherits(cv, "gp3bayes_kfold_cv"), identical(cv$automatic_selection, FALSE))
}

cat("SPECIFICATION CLOSURE REAL-BACKEND SMOKE TEST PASSED\n")

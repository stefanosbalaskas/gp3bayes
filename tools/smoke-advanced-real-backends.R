#!/usr/bin/env Rscript

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
pkgload::load_all(root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

run_rstan <- identical(Sys.getenv("GP3BAYES_SMOKE_RSTAN"), "1")
run_cmdstanr <- identical(Sys.getenv("GP3BAYES_SMOKE_CMDSTANR"), "1")
run_priorsense <- identical(Sys.getenv("GP3BAYES_SMOKE_PRIORSENSE"), "1")
run_sbc <- identical(Sys.getenv("GP3BAYES_SMOKE_SBC"), "1")

if (!any(c(run_rstan, run_cmdstanr, run_priorsense, run_sbc))) {
  cat(
    "No real-backend smoke requested.\n",
    "Set one or more of:\n",
    "  GP3BAYES_SMOKE_RSTAN=1\n",
    "  GP3BAYES_SMOKE_CMDSTANR=1\n",
    "  GP3BAYES_SMOKE_PRIORSENSE=1\n",
    "  GP3BAYES_SMOKE_SBC=1\n",
    sep = ""
  )
  quit(status = 0)
}

simulation <- simulate_hierarchical_binary_data(
  n_participants = 10,
  trials_per_participant = 6,
  n_items = 5,
  random_slope_sd = 0,
  seed = 2026
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
  simulation$data,
  contract,
  condition_levels = c("control", "treatment")
)
specification <- specify_binary_model_with_interaction_prior(
  prepared,
  baseline = 0.35
)

fit <- NULL

if (run_rstan) {
  fit <- fit_binary_model_backend(
    specification,
    backend = "rstan",
    chains = 2,
    iter = 600,
    warmup = 300,
    cores = 2,
    seed = 2026
  )
  stopifnot(inherits(fit, "gp3bayes_binary_fit"))
}

if (run_cmdstanr) {
  fit <- fit_binary_model_backend(
    specification,
    backend = "cmdstanr",
    chains = 2,
    iter = 600,
    warmup = 300,
    cores = 2,
    seed = 2026
  )
  stopifnot(inherits(fit, "gp3bayes_binary_fit"))
}

if (run_priorsense) {
  if (is.null(fit)) {
    stop("Enable an rstan or cmdstanr fit for the priorsense smoke.")
  }
  sensitivity <- assess_powerscaled_sensitivity(fit)
  stopifnot(inherits(
    sensitivity,
    "gp3bayes_powerscale_sensitivity"
  ))
}

if (run_sbc) {
  plan <- create_brms_sbc_plan(
    specification,
    n_sims = 2,
    backend = if (run_cmdstanr) "cmdstanr" else "rstan",
    chains = 1,
    iter = 400,
    warmup = 200,
    generator_iter = 800,
    generator_warmup = 400,
    seed = 2026
  )
  result <- run_sbc_plan(plan, cores_per_fit = 1)
  stopifnot(inherits(result, "gp3bayes_sbc_result"))
}

cat("ADVANCED REAL-BACKEND SMOKE TEST PASSED\n")

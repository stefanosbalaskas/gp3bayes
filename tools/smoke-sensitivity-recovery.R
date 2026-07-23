setwd(
  "C:/Users/Stefanos-PC/Documents/Rstudio/gp3bayes"
)

stopifnot(
  identical(
    system2(
      "git",
      c(
        "branch",
        "--show-current"
      ),
      stdout = TRUE
    ),
    "feature/integrated-documentation"
  ),
  requireNamespace(
    "brms",
    quietly = TRUE
  ),
  requireNamespace(
    "rstan",
    quietly = TRUE
  ),
  requireNamespace(
    "posterior",
    quietly = TRUE
  )
)

pkgload::load_all(
  path = ".",
  reset = TRUE,
  export_all = FALSE
)

binary_simulation <-
  simulate_hierarchical_binary_data(
    n_participants = 8,
    trials_per_participant = 8,
    n_items = 4,
    random_slope_sd = 0,
    seed = 8001
  )
binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition"
)
binary_prepared <-
  prepare_hierarchical_binary_data(
    binary_simulation$data,
    binary_contract,
    condition_levels = c(
      "control",
      "treatment"
    )
  )
binary_specification <-
  specify_binary_model(
    binary_prepared,
    baseline = 0.35
  )
binary_fit <- fit_binary_model(
  binary_specification,
  chains = 2,
  iter = 300,
  warmup = 150,
  cores = 2,
  seed = 8002,
  refresh = 0
)

binary_sensitivity <-
  assess_binary_prior_sensitivity(
    binary_fit,
    scale_multipliers = c(
      tighter = 0.75,
      wider = 1.25
    ),
    chains = 2,
    iter = 300,
    warmup = 150,
    cores = 2,
    seed = 8003,
    refresh = 0
  )

binary_recovery <- run_binary_recovery(
  repetitions = 2,
  n_participants = 8,
  trials_per_participant = 8,
  n_items = 4,
  random_slope = FALSE,
  chains = 2,
  iter = 300,
  warmup = 150,
  cores = 2,
  seed = 8010,
  refresh = 0,
  maximum_standardized_bias = 100,
  minimum_coverage = 0,
  minimum_diagnostic_pass_fraction = 0,
  continue_on_error = FALSE
)

duration_simulation <-
  simulate_hierarchical_duration_data(
    n_participants = 8,
    trials_per_participant = 8,
    n_items = 4,
    random_slope_sd = 0,
    seed = 9001
  )
duration_contract <- create_model_contract(
  family = "duration",
  outcome_col = "duration",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit = "milliseconds"
)
duration_prepared <-
  prepare_hierarchical_duration_data(
    duration_simulation$data,
    duration_contract,
    condition_levels = c(
      "control",
      "treatment"
    )
  )
duration_specification <-
  specify_duration_model(
    duration_prepared,
    baseline = 500
  )
duration_fit <- fit_duration_model(
  duration_specification,
  chains = 2,
  iter = 300,
  warmup = 150,
  cores = 2,
  seed = 9002,
  refresh = 0
)

duration_sensitivity <-
  assess_duration_prior_sensitivity(
    duration_fit,
    scale_multipliers = c(
      tighter = 0.75,
      wider = 1.25
    ),
    chains = 2,
    iter = 300,
    warmup = 150,
    cores = 2,
    seed = 9003,
    refresh = 0
  )

duration_recovery <- run_duration_recovery(
  repetitions = 2,
  n_participants = 8,
  trials_per_participant = 8,
  n_items = 4,
  random_slope = FALSE,
  chains = 2,
  iter = 300,
  warmup = 150,
  cores = 2,
  seed = 9010,
  refresh = 0,
  maximum_standardized_bias = 100,
  minimum_coverage = 0,
  minimum_diagnostic_pass_fraction = 0,
  continue_on_error = FALSE
)

stopifnot(
  inherits(
    binary_sensitivity,
    "gp3bayes_binary_prior_sensitivity"
  ),
  binary_sensitivity$status %in%
    c(
      "pass",
      "review",
      "fail"
    ),
  inherits(
    binary_recovery,
    "gp3bayes_binary_recovery"
  ),
  identical(
    binary_recovery$status,
    "review"
  ),
  !isTRUE(
    binary_recovery$validation_claim
  ),
  inherits(
    duration_sensitivity,
    "gp3bayes_duration_prior_sensitivity"
  ),
  inherits(
    duration_recovery,
    "gp3bayes_duration_recovery"
  ),
  identical(
    duration_recovery$status,
    "review"
  ),
  !isTRUE(
    duration_recovery$validation_claim
  )
)

print(binary_sensitivity)
print(binary_recovery)
print(duration_sensitivity)
print(duration_recovery)

cat(
  "Extended sensitivity and recovery smoke test passed.\n"
)
cat(
  "Two-repetition recovery runs remained review-only as required.\n"
)

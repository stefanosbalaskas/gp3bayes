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
    "feature/duration-workflow"
  ),
  requireNamespace(
    "brms",
    quietly = TRUE
  ),
  requireNamespace(
    "rstan",
    quietly = TRUE
  )
)

pkgload::load_all(
  path = ".",
  reset = TRUE,
  export_all = FALSE
)

simulation <-
  simulate_hierarchical_duration_data(
    n_participants = 8,
    trials_per_participant = 8,
    n_items = 4,
    random_slope_sd = 0,
    seed = 4201
  )

contract <- create_model_contract(
  family = "duration",
  outcome_col = "duration",
  participant_col =
    "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit =
    "milliseconds"
)

prepared <-
  prepare_hierarchical_duration_data(
    simulation$data,
    contract,
    condition_levels = c(
      "control",
      "treatment"
    )
  )

specification <- specify_duration_model(
  prepared,
  baseline = 500
)

prior_predictive <-
  check_duration_prior_predictive(
    specification,
    draws = 100,
    seed = 4202
  )

translation <-
  translate_duration_model_to_brms(
    specification
  )

fit <- fit_duration_model(
  specification,
  chains = 2L,
  iter = 400L,
  warmup = 200L,
  cores = 2L,
  seed = 4203L,
  adapt_delta = 0.90,
  max_treedepth = 10L,
  refresh = 0L
)

stopifnot(
  inherits(
    simulation,
    "gp3bayes_duration_simulation"
  ),
  all(
    simulation$data$duration > 0
  ),
  inherits(
    prepared,
    "gp3bayes_duration_prepared"
  ),
  inherits(
    specification,
    "gp3bayes_duration_model_specification"
  ),
  inherits(
    prior_predictive,
    "gp3bayes_duration_prior_predictive_check"
  ),
  inherits(
    translation,
    "gp3bayes_duration_backend_specification"
  ),
  inherits(
    fit,
    "gp3bayes_duration_fit"
  ),
  inherits(
    fit$backend_fit,
    "brmsfit"
  ),
  inherits(
    fit$backend_fit$fit,
    "stanfit"
  ),
  !isTRUE(
    fit$diagnostics_assessed
  ),
  !isTRUE(
    fit$posterior_adequacy_established
  )
)

print(simulation)
print(prepared)
print(specification)
print(prior_predictive)
print(translation)
print(fit)

cat(
  "Restricted lognormal duration workflow smoke test passed.\n"
)
cat(
  "No convergence or posterior-adequacy claim was made.\n"
)

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
    "feature/binary-posterior-validation"
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

simulation <- simulate_hierarchical_binary_data(
  n_participants = 8,
  trials_per_participant = 8,
  n_items = 4,
  random_slope_sd = 0,
  seed = 4101
)

contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c(
    "control",
    "treatment"
  )
)

specification <- specify_binary_model(
  prepared,
  baseline = 0.35
)

fit <- fit_binary_model(
  specification,
  chains = 2L,
  iter = 400L,
  warmup = 200L,
  cores = 2L,
  seed = 4102L,
  adapt_delta = 0.90,
  max_treedepth = 10L,
  refresh = 0L
)

diagnostics <- diagnose_binary_fit(
  fit
)
posterior_summary <- summarise_binary_posterior(
  fit
)
posterior_predictive <-
  check_binary_posterior_predictive(
    fit,
    draws = 100L,
    seed = 4103L
  )

report_file <- tempfile(
  pattern = "gp3bayes-binary-report-",
  fileext = ".md"
)

report <- create_binary_model_report(
  fit,
  diagnostics = diagnostics,
  posterior_summary =
    posterior_summary,
  posterior_predictive =
    posterior_predictive,
  file = report_file
)

stopifnot(
  inherits(
    diagnostics,
    "gp3bayes_binary_diagnostics"
  ),
  diagnostics$status %in%
    c(
      "pass",
      "review",
      "fail"
    ),
  !isTRUE(
    diagnostics$convergence_claim
  ),
  inherits(
    posterior_summary,
    "gp3bayes_binary_posterior_summary"
  ),
  inherits(
    posterior_predictive,
    "gp3bayes_binary_posterior_predictive_check"
  ),
  !isTRUE(
    posterior_predictive$adequacy_established
  ),
  inherits(
    report,
    "gp3bayes_binary_model_report"
  ),
  file.exists(
    report$file
  )
)

print(diagnostics)
print(posterior_summary)
print(posterior_predictive)
print(report)

cat(
  "Binary posterior validation smoke test passed.\n"
)
cat(
  "No automatic convergence or posterior-adequacy claim was made.\n"
)

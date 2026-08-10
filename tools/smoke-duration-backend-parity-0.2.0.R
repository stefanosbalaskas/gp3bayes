# Optional real-backend parity smoke test for the duration family.
# Run only after both rstan and cmdstanr environments validate successfully.

stopifnot(file.exists("DESCRIPTION"))
devtools::load_all(quiet = TRUE)

validate_backend_environment("rstan", compile_test = TRUE, strict = TRUE)
validate_backend_environment("cmdstanr", compile_test = TRUE, strict = TRUE)

simulation <- simulate_hierarchical_duration_data(
  n_participants = 12,
  trials_per_participant = 8,
  n_items = 6,
  random_slope_sd = 0,
  seed = 9300
)
contract <- create_model_contract(
  "duration", "duration", "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit = "milliseconds"
)
prepared <- prepare_hierarchical_duration_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment")
)
specification <- specify_duration_model(prepared, baseline = 500)

fit_rstan <- fit_duration_model_backend(
  specification,
  backend = "rstan",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 9301,
  refresh = 0
)
fit_cmdstanr <- fit_duration_model_backend(
  specification,
  backend = "cmdstanr",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 9301,
  refresh = 0
)


# Real duration backend sampling diagnostics.
diagnostics_rstan <- diagnose_model_fit(fit_rstan)
diagnostics_cmdstanr <- diagnose_model_fit(fit_cmdstanr)

print(diagnostics_rstan)
print(diagnostics_cmdstanr)

if (!identical(diagnostics_rstan$status, "pass")) {
  stop(
    "Duration rstan sampling diagnostics require review.",
    call. = FALSE
  )
}

if (!identical(diagnostics_cmdstanr$status, "pass")) {
  stop(
    "Duration cmdstanr sampling diagnostics require review.",
    call. = FALSE
  )
}

posterior_variables <- grep(
  "^(b_|sigma$)",
  posterior::variables(posterior::as_draws_array(fit_rstan$backend_fit)),
  value = TRUE
)
if (!length(posterior_variables)) {
  stop("No duration posterior variables were found.", call. = FALSE)
}
posterior_variables <- posterior_variables[
  seq_len(min(3L, length(posterior_variables)))
]

parity <- audit_backend_parity(
  fit_rstan,
  fit_cmdstanr,
  variables = posterior_variables,
  mcse_multiplier = 4,
  relative_sd_tolerance = 0.20
)
print(parity)

if (!identical(parity$status, "pass")) {
  stop(
    "Duration backend parity requires review; inspect `parity$table`.",
    call. = FALSE
  )
}

cat("Real duration rstan/cmdstanr parity smoke test: PASS\n")

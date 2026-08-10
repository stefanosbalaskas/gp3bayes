# Optional real-backend parity smoke test.
# Run only after both rstan and cmdstanr environments validate successfully.

stopifnot(file.exists("DESCRIPTION"))
devtools::load_all(quiet = TRUE)

rstan_env <- validate_backend_environment("rstan", compile_test = TRUE, strict = TRUE)
cmdstan_env <- validate_backend_environment("cmdstanr", compile_test = TRUE, strict = TRUE)

simulation <- simulate_hierarchical_binary_data(
  n_participants = 12,
  trials_per_participant = 8,
  n_items = 6,
  random_slope_sd = 0,
  seed = 9200
)
contract <- create_model_contract(
  "binary", "selected", "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition"
)
prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment")
)
specification <- specify_binary_model(prepared, baseline = 0.35)

fit_rstan <- fit_binary_model_backend(
  specification,
  backend = "rstan",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 9201,
  refresh = 0
)
fit_cmdstanr <- fit_binary_model_backend(
  specification,
  backend = "cmdstanr",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 9201,
  refresh = 0
)


# Real binary backend sampling diagnostics.
diagnostics_rstan <- diagnose_model_fit(fit_rstan)
diagnostics_cmdstanr <- diagnose_model_fit(fit_cmdstanr)

print(diagnostics_rstan)
print(diagnostics_cmdstanr)

if (!identical(diagnostics_rstan$status, "pass")) {
  stop(
    "Binary rstan sampling diagnostics require review.",
    call. = FALSE
  )
}

if (!identical(diagnostics_cmdstanr$status, "pass")) {
  stop(
    "Binary cmdstanr sampling diagnostics require review.",
    call. = FALSE
  )
}

posterior_variables <- grep(
  "^b_",
  posterior::variables(posterior::as_draws_array(fit_rstan$backend_fit)),
  value = TRUE
)
if (!length(posterior_variables)) {
  stop("No population-level posterior variables were found.", call. = FALSE)
}
posterior_variables <- posterior_variables[seq_len(min(2L, length(posterior_variables)))]

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
    "Real-backend parity smoke test requires review; inspect `parity$table`.",
    call. = FALSE
  )
}

cat("Real rstan/cmdstanr parity smoke test: PASS\n")

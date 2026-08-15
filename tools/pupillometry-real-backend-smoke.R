# gp3bayes 0.4.0.9000 real-backend pupillometry smoke validation
#
# Manual validation only. This script intentionally performs real Stan fits and
# is NOT sourced by package tests or ordinary examples.

project_root <- "C:/Users/Stefanos-PC/Documents/Rstudio/gp3bayes"
expected_branch <- "feature/bayesian-pupillometry-foundation"
expected_version <- "0.4.0.9000"

git <- function(args) {
  out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("git command failed: ", paste(out, collapse = "\n"), call. = FALSE)
  }
  out
}
version_of <- function() {
  x <- readLines("DESCRIPTION", warn = FALSE, encoding = "UTF-8")
  trimws(sub("^Version:", "", grep("^Version:", x, value = TRUE)))
}

if (!dir.exists(project_root)) stop("gp3bayes project root not found.", call. = FALSE)
setwd(project_root)
if (!identical(git(c("rev-parse", "--abbrev-ref", "HEAD")), expected_branch)) {
  stop("Run this smoke script only on the pupillometry feature branch.", call. = FALSE)
}
if (!identical(version_of(), expected_version)) {
  stop("Expected gp3bayes version 0.4.0.9000.", call. = FALSE)
}
for (pkg in c("devtools", "brms", "rstan", "cmdstanr", "posterior")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package is not installed: ", pkg, call. = FALSE)
  }
}
cmdstan <- try(cmdstanr::cmdstan_path(), silent = TRUE)
if (inherits(cmdstan, "try-error") || !nzchar(cmdstan)) {
  stop("A working CmdStan installation is required.", call. = FALSE)
}

devtools::load_all(quiet = TRUE)

cat("===== deterministic synthetic pupil dataset =====\n")
sim <- simulate_pupil_timecourse(
  n_participants = 6L,
  trials_per_participant = 4L,
  n_items = 6L,
  sampling_frequency = 30,
  time_window = c(-0.3, 1.5),
  baseline_window = c(-0.3, 0),
  conditions = c("control", "treatment"),
  baseline_pupil = 4,
  response_amplitude = 0.35,
  condition_difference = 0.12,
  residual_sd = 0.07,
  ar1 = 0.35,
  blink_trial_probability = 0.10,
  include_gaze = TRUE,
  include_luminance = TRUE,
  seed = 2404
)

contract <- create_pupil_contract(
  outcome_col = "pupil_mm",
  participant_col = "participant_id",
  trial_col = "trial_id",
  item_col = "item_id",
  condition_col = "condition",
  timestamp_col = "timestamp",
  time_col = "event_time",
  pupil_unit = "millimetres",
  sampling_frequency = 30,
  time_unit = "seconds",
  eye = "combined",
  validity_col = "valid",
  interpolation_col = "interpolated",
  blink_col = "blink",
  gaze_x_col = "gaze_x",
  gaze_y_col = "gaze_y",
  luminance_col = "luminance",
  baseline_window = c(-0.3, 0),
  baseline_method = "none",
  baseline_applied = FALSE,
  pfe_corrected = FALSE,
  source_vendor = "Gazepoint",
  device_model = "synthetic Gazepoint-like contract",
  preprocessing_provenance = "gp3bayes deterministic simulator",
  upstream_package = "gp3bayes",
  upstream_version = expected_version
)

prepared <- prepare_pupil_timecourse(
  sim$data,
  contract,
  baseline_operation = "subtract",
  baseline_window = c(-0.3, 0),
  output_unit = "millimetres"
)
readiness <- audit_pupil_readiness(prepared)
stopifnot(inherits(readiness, "gp3bayes_pupil_readiness"))

spec <- specify_pupil_timecourse_model(
  prepared,
  temporal_structure = "smooth",
  smooth_basis_dimension = 6L,
  condition_trajectory = TRUE,
  autocorrelation = "ar1",
  participant_trajectory = "none",
  item_effects = TRUE,
  covariates = "luminance"
)
translation <- translate_pupil_model_to_brms(spec)
stopifnot(
  identical(spec$unrestricted_formula, FALSE),
  identical(spec$unrestricted_family, FALSE),
  identical(translation$unrestricted_formula, FALSE),
  identical(translation$unrestricted_family, FALSE)
)

prior_plan <- check_pupil_prior_predictive(
  spec,
  execute = FALSE,
  backend = "rstan",
  draws = 100L,
  chains = 2L,
  iter = 600L,
  warmup = 300L,
  cores = 2L,
  seed = 2405
)
stopifnot(!isTRUE(prior_plan$executed))

fit_args <- list(
  specification = spec,
  chains = 4L,
  iter = 1600L,
  warmup = 800L,
  cores = 2L,
  seed = 2406,
  adapt_delta = 0.99,
  max_treedepth = 12L,
  refresh = 0L
)

cat("===== rstan pupil fit =====\n")
fit_rstan <- do.call(
  fit_pupil_model_backend,
  c(fit_args, list(backend = "rstan"))
)
cat("rstan pupil fit: PASS (fit object created; adequacy not inferred)\n")

cat("===== cmdstanr pupil fit =====\n")
fit_cmdstanr <- do.call(
  fit_pupil_model_backend,
  c(fit_args, list(backend = "cmdstanr"))
)
cat("cmdstanr pupil fit: PASS (fit object created; adequacy not inferred)\n")

wrapper_fields <- c(
  "fit_version", "family", "model_family", "specification", "translation",
  "backend_fit", "outcome_unit", "backend_interface", "sampling_backend",
  "algorithm", "sampling", "package_versions", "unrestricted_formula",
  "unrestricted_family", "fit_performed", "convergence_established",
  "posterior_adequacy_established", "causal_identification_established"
)
stopifnot(
  identical(sort(intersect(names(fit_rstan), wrapper_fields)), sort(wrapper_fields)),
  identical(sort(intersect(names(fit_cmdstanr), wrapper_fields)), sort(wrapper_fields)),
  identical(fit_rstan$specification$formula_text, fit_cmdstanr$specification$formula_text),
  identical(fit_rstan$specification$priors, fit_cmdstanr$specification$priors)
)
cat("backend wrapper/schema parity: PASS\n")

cat("===== estimands and prediction =====\n")
pred_rstan <- predict_pupil_trajectory(
  fit_rstan, type = "expected", ndraws = 200L, population_only = TRUE
)
pred_cmdstanr <- predict_pupil_trajectory(
  fit_cmdstanr, type = "expected", ndraws = 200L, population_only = TRUE
)
traj_rstan <- estimate_pupil_trajectory(pred_rstan, probability = 0.90)
traj_cmdstanr <- estimate_pupil_trajectory(pred_cmdstanr, probability = 0.90)
window_rstan <- estimate_pupil_window(pred_rstan, c(0.3, 1.2), probability = 0.90)
auc_rstan <- estimate_pupil_auc(pred_rstan, c(0.3, 1.2), probability = 0.90)
peak_rstan <- estimate_pupil_peak(pred_rstan, c(0.3, 1.2), probability = 0.90)
latency_rstan <- estimate_pupil_peak_latency(pred_rstan, c(0.3, 1.2), probability = 0.90)
contrast_rstan <- pupil_condition_contrast(
  pred_rstan,
  contrast = c("treatment", "control"),
  threshold = 0.05,
  probability = 0.90
)
stopifnot(
  nrow(pupil_trajectory_table(traj_rstan)) > 0L,
  nrow(pupil_trajectory_table(traj_cmdstanr)) > 0L,
  nrow(as.data.frame(window_rstan)) > 0L,
  nrow(as.data.frame(auc_rstan)) > 0L,
  nrow(as.data.frame(peak_rstan)) > 0L,
  nrow(as.data.frame(latency_rstan)) > 0L,
  nrow(as.data.frame(contrast_rstan)) > 0L
)
cat("trajectory/window/AUC/peak/latency/contrast extraction: PASS\n")

cat("===== posterior predictive and temporal diagnostics =====\n")
ppc_rstan <- check_pupil_posterior_predictive(
  fit_rstan, ndraws = 100L, probability = 0.90, window = c(0.3, 1.2)
)
diag_rstan <- diagnose_pupil_fit(fit_rstan)
diag_cmdstanr <- diagnose_pupil_fit(fit_cmdstanr)
acf_rstan <- pupil_residual_acf(fit_rstan, max_lag = 8L)
stopifnot(
  inherits(ppc_rstan, "gp3bayes_pupil_ppc"),
  inherits(diag_rstan, "gp3bayes_pupil_diagnostics"),
  inherits(diag_cmdstanr, "gp3bayes_pupil_diagnostics"),
  is.data.frame(acf_rstan)
)
cat("PPC and temporal diagnostic extraction: PASS (evidence only)\n")

numerical_metrics <- c(
  "max_rhat",
  "min_bulk_ess",
  "min_tail_ess",
  "divergent_transitions",
  "max_treedepth_hits",
  "minimum_ebfmi"
)

check_numerical_diagnostics <- function(x, backend) {
  tab <- x$evidence[
    x$evidence$metric %in% numerical_metrics,
    c("metric", "value", "status"),
    drop = FALSE
  ]
  tab <- tab[match(numerical_metrics, tab$metric), , drop = FALSE]
  cat("\n", backend, " numerical sampler diagnostics:\n", sep = "")
  print(tab)
  missing <- numerical_metrics[!numerical_metrics %in% tab$metric]
  if (length(missing)) {
    stop(
      backend,
      " diagnostics missing required metric(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  review <- tab$metric[tab$status != "pass" | is.na(tab$status)]
  if (length(review)) {
    stop(
      backend,
      " numerical sampler gate remains REVIEW: ",
      paste(review, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(tab)
}

numerical_rstan <- check_numerical_diagnostics(
  diag_rstan,
  "rstan"
)
numerical_cmdstanr <- check_numerical_diagnostics(
  diag_cmdstanr,
  "cmdstanr"
)

cat("real-backend numerical sampler diagnostics: PASS\n")

cat("===== grouped validation target =====\n")
validation_plan <- create_pupil_validation_plan(
  fit_rstan,
  target = "new_trial_known_participant",
  K = 2L,
  seed = 2407
)
stopifnot(!isTRUE(validation_plan$leakage_detected))
validation_plan_result <- validate_pupil_model(
  fit_rstan, validation_plan, execute = FALSE
)
stopifnot(!isTRUE(validation_plan_result$executed))
cat("grouped trial validation plan/no-leakage audit: PASS\n")

run_grouped_cv <- identical(
  tolower(Sys.getenv("GP3BAYES_PUPIL_RUN_GROUPED_CV", unset = "false")),
  "true"
)
if (run_grouped_cv) {
  cat("Executing exact two-fold grouped CV because GP3BAYES_PUPIL_RUN_GROUPED_CV=true.\n")
  grouped_cv <- validate_pupil_model(
    fit_rstan, validation_plan, execute = TRUE, ndraws = 100L
  )
  stopifnot(isTRUE(grouped_cv$executed))
  cat("exact grouped trial K-fold: PASS (predictive evidence only)\n")
} else {
  cat("exact grouped trial K-fold: NOT RUN by default; plan validated without leakage.\n")
}

cat("===== posterior backend comparison =====\n")
sum_rstan <- summarise_pupil_posterior(fit_rstan)$table
sum_cmdstanr <- summarise_pupil_posterior(fit_cmdstanr)$table
common <- intersect(sum_rstan$variable, sum_cmdstanr$variable)
common <- common[grepl("^(b_|sd_|sds_|ar\\[|sigma$)", common)]
if (!length(common)) {
  stop("No common declared posterior parameters found for backend comparison.", call. = FALSE)
}
r <- sum_rstan[match(common, sum_rstan$variable), ]
c <- sum_cmdstanr[match(common, sum_cmdstanr$variable), ]
denom <- pmax(r$sd, c$sd, 1e-8)
standardized_mean_difference <- abs(r$mean - c$mean) / denom
parity_table <- data.frame(
  variable = common,
  rstan_mean = r$mean,
  cmdstanr_mean = c$mean,
  pooled_scale = denom,
  standardized_mean_difference = standardized_mean_difference,
  stringsAsFactors = FALSE
)
print(parity_table)
max_difference <- max(standardized_mean_difference, na.rm = TRUE)
if (is.finite(max_difference) && max_difference <= 0.50) {
  cat("posterior backend parity: PASS under declared smoke tolerance (<= 0.50 posterior SD)\n")
} else {
  cat(
    "posterior backend parity: REVIEW; maximum standardized mean difference = ",
    signif(max_difference, 4),
    ". This is not automatically a backend failure; inspect Monte Carlo diagnostics and rerun if needed.\n",
    sep = ""
  )
}

cat("===== smoke validation summary =====\n")
cat("rstan pupil fit: PASS\n")
cat("cmdstanr pupil fit: PASS\n")
cat("backend object schema parity: PASS\n")
cat("posterior backend parity: see PASS/REVIEW statement above\n")
cat("No causal, psychological, or model-adequacy conclusion is produced by this script.\n")

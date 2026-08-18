# Optional real-backend validation for gp3bayes 0.5.0.9000.
# Defines a function only; sourcing does not fit Stan models.

run_gp3bayes_0_5_real_backend_smoke <- function(
    project_root = ".",
    run_rstan = TRUE,
    run_cmdstanr = TRUE,
    run_gp = TRUE,
    run_measurement = TRUE,
    run_binocular = TRUE,
    run_response_shape = FALSE,
    chains = 2L,
    iter = 1200L,
    warmup = 600L,
    cores = 2L,
    seed = 2050) {

  project_root <- normalizePath(project_root, mustWork = TRUE)
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(project_root)
  if (!requireNamespace("devtools", quietly = TRUE)) stop("Install devtools.", call. = FALSE)
  if (!requireNamespace("brms", quietly = TRUE)) stop("Install brms.", call. = FALSE)
  if (!requireNamespace("posterior", quietly = TRUE)) stop("Install posterior.", call. = FALSE)
  if (cores < 1L || cores > 2L) stop("`cores` must be 1 or 2.", call. = FALSE)
  if (!run_rstan && !run_cmdstanr) stop("Enable at least one backend.", call. = FALSE)
  d <- read.dcf("DESCRIPTION")
  if (!identical(unname(d[1L, "Version"]), "0.5.0.9000")) stop("Expected gp3bayes 0.5.0.9000.", call. = FALSE)
  devtools::load_all(".", quiet = TRUE)

  if (run_rstan && !requireNamespace("rstan", quietly = TRUE)) stop("run_rstan=TRUE but rstan is unavailable.", call. = FALSE)
  if (run_cmdstanr) {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("run_cmdstanr=TRUE but cmdstanr is unavailable.", call. = FALSE)
    cmdstan_ver <- tryCatch(cmdstanr::cmdstan_version(), error = function(e) NULL)
    if (is.null(cmdstan_ver)) stop("CmdStan is not configured.", call. = FALSE)
  }

  cat("\n============================================================\nREAL BACKEND SMOKE — gp3bayes 0.5\n============================================================\n")
  cat("R: ", R.version.string, "\n", sep = "")
  cat("brms: ", as.character(utils::packageVersion("brms")), "\n", sep = "")
  if (run_rstan) cat("rstan: ", as.character(utils::packageVersion("rstan")), "\n", sep = "")
  if (run_cmdstanr) cat("cmdstanr: ", as.character(utils::packageVersion("cmdstanr")), " | CmdStan: ", as.character(cmdstan_ver), "\n", sep = "")

  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 8,
    trials_per_participant = 2,
    time_points = 11,
    time_range = c(-100, 900),
    family = "gaussian",
    residual_scale = 0.08,
    ar = c(0.35),
    seed = seed
  )
  base_spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    temporal_structure = "linear",
    family = "gaussian",
    residual_scale = "time",
    autocorrelation = "ar1",
    participant_trajectory = "none",
    item_effects = FALSE,
    predictive_target = "future_segment"
  )
  audit <- audit_advanced_pupil_identifiability(base_spec)
  print(audit)

  common_args <- list(
    specification = base_spec,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    adapt_delta = 0.97,
    max_treedepth = 13L,
    refresh = 0L
  )
  fits <- list()
  if (run_rstan) {
    cat("\n--- rstan Gaussian distributional AR1 ---\n")
    fits$rstan <- do.call(fit_advanced_pupil_model_backend, c(common_args, list(backend = "rstan")))
    print(diagnose_advanced_pupil_fit(fits$rstan))
  }
  if (run_cmdstanr) {
    cat("\n--- cmdstanr Gaussian distributional AR1 ---\n")
    fits$cmdstanr <- do.call(fit_advanced_pupil_model_backend, c(common_args, list(backend = "cmdstanr")))
    print(diagnose_advanced_pupil_fit(fits$cmdstanr))
  }

  parity <- NULL
  if (all(c("rstan", "cmdstanr") %in% names(fits))) {
    dr <- posterior::as_draws_df(fits$rstan$backend_fit)
    dc <- posterior::as_draws_df(fits$cmdstanr$backend_fit)
    vars <- intersect(names(dr), names(dc))
    vars <- vars[grepl("^(b_|sigma$|ar\\[|b_sigma_|nu$)", vars)]
    if (length(vars)) {
      parity <- do.call(rbind, lapply(vars, function(v) {
        mr <- mean(dr[[v]]); mc <- mean(dc[[v]])
        sr <- stats::sd(dr[[v]]); sc <- stats::sd(dc[[v]])
        pooled <- sqrt((sr^2 + sc^2) / 2)
        data.frame(parameter = v, mean_rstan = mr, mean_cmdstanr = mc, pooled_sd = pooled,
                   standardized_mean_difference = if (is.finite(pooled) && pooled > 0) abs(mr - mc) / pooled else NA_real_)
      }))
      rownames(parity) <- NULL
      cat("\nBackend parity (shared governed parameters):\n")
      print(parity, row.names = FALSE)
      mx <- max(parity$standardized_mean_difference, na.rm = TRUE)
      if (!is.finite(mx) || mx > 0.50) stop("Backend parity exceeded 0.50 posterior SD.", call. = FALSE)
      cat("Maximum standardized backend difference: ", format(mx, digits = 5), " posterior SD — PASS\n", sep = "")
    }
  }

  extras <- list()
  backend_extra <- if (run_cmdstanr) "cmdstanr" else "rstan"

  if (run_gp) {
    cat("\n--- robust approximate Matérn GP ---\n")
    gp_sim <- simulate_advanced_pupil_timecourse(
      n_participants = 8, trials_per_participant = 2, time_points = 11, time_range = c(-100, 900),
      family = "student", residual_scale = 0.08, outlier_fraction = 0.02, seed = seed + 10L
    )
    gp_spec <- specify_advanced_pupil_timecourse_model(
      gp_sim$data,
      temporal_structure = "gaussian_process",
      family = "student",
      residual_scale = "condition",
      gp_spec = create_pupil_gp_spec("matern32", basis = "approximate", k = 10),
      autocorrelation = "none",
      item_effects = FALSE
    )
    extras$gp <- fit_advanced_pupil_model_backend(
      gp_spec, backend = backend_extra, chains = chains, iter = iter, warmup = warmup,
      cores = cores, seed = seed + 10L, adapt_delta = 0.97, max_treedepth = 13L, refresh = 0L
    )
    print(diagnose_advanced_pupil_fit(extras$gp))
    stopifnot(nrow(pupil_gp_table(pupil_gp_hyperparameters(extras$gp))) >= 1L)
  }

  if (run_measurement) {
    cat("\n--- predictor measurement uncertainty + missing predictor ---\n")
    m_sim <- simulate_advanced_pupil_timecourse(
      n_participants = 8, trials_per_participant = 2, time_points = 11, time_range = c(-100, 900),
      measurement_error_sd = 0.02, seed = seed + 20L
    )
    # Induce a small predictor-missingness pattern; do not interpolate.
    # 0.5 real-smoke measurement DGP: non-degenerate latent baseline
    # The package simulator intentionally generates a participant-level latent baseline.
    # For the real measurement-error smoke, add genuine row-level latent
    # variation so the latent-response residual sigma is not validated at
    # a near-zero boundary.
    set.seed(seed + 22L)
    measurement_latent_sd <- 0.08
    m_sim$data$baseline_pupil <-
      m_sim$data$baseline_pupil +
      stats::rnorm(
        nrow(m_sim$data),
        mean = 0,
        sd = measurement_latent_sd
      )

    measurement_participant_mean <- stats::ave(
      m_sim$data$baseline_pupil,
      m_sim$data$participant_id,
      FUN = function(z) mean(z, na.rm = TRUE)
    )

    measurement_within_sd <- stats::sd(
      m_sim$data$baseline_pupil - measurement_participant_mean,
      na.rm = TRUE
    )

    measurement_noise_sd <- stats::median(
      m_sim$data$baseline_se,
      na.rm = TRUE
    )

    stopifnot(
      is.finite(measurement_within_sd),
      is.finite(measurement_noise_sd),
      measurement_noise_sd > 0,
      measurement_within_sd > 1.5 * measurement_noise_sd
    )

    set.seed(seed + 21L)
    ii <- sample.int(nrow(m_sim$data), max(2L, floor(0.03 * nrow(m_sim$data))))
    m_sim$data$baseline_pupil[ii] <- NA_real_
    mm <- create_pupil_measurement_model(baseline_error = "baseline_se")
    ms <- create_pupil_missingness_spec(response = "exclude", predictors = "baseline_pupil", auxiliary_predictors = "luminance")
    m_spec <- specify_advanced_pupil_timecourse_model(
      m_sim$data,
      temporal_structure = "linear", family = "gaussian", autocorrelation = "none",
      covariates = c("baseline_pupil", "luminance"), measurement_model = mm, missingness_model = ms,
      item_effects = FALSE
    )
    extras$measurement <- fit_advanced_pupil_model_backend(
      m_spec, backend = backend_extra, chains = chains, iter = iter, warmup = warmup,
      cores = cores, seed = seed + 20L, adapt_delta = 0.97, max_treedepth = 13L, refresh = 0L
    )
    print(diagnose_advanced_pupil_fit(extras$measurement))
  }

  if (run_binocular) {
    cat("\n--- joint binocular model ---\n")
    bsim <- simulate_binocular_pupil_timecourse(n_participants = 8, trials_per_participant = 2, time_points = 11, time_range = c(-100, 900), seed = seed + 30L)
    bprep <- prepare_binocular_pupil_timecourse(bsim$data)
    bspec <- specify_binocular_pupil_model(bprep, family = "gaussian", temporal_structure = "linear", residual_correlation = TRUE)
    extras$binocular <- fit_binocular_pupil_model(
      bspec, backend = backend_extra, chains = chains, iter = iter, warmup = warmup,
      cores = cores, seed = seed + 30L, adapt_delta = 0.97, max_treedepth = 16L, refresh = 0L
    )
    print(extras$binocular)
    bc <- pupil_binocular_correlation(extras$binocular)
    print(bc)
  }

  if (run_response_shape) {
    cat("\n--- experimental nonlinear response shape ---\n")
    rsim <- simulate_pupil_response_shape(n_participants = 10, trials_per_participant = 2, time_points = 17, seed = seed + 40L)
    rspec <- specify_pupil_response_shape_model(rsim$data, family = "gaussian")
    extras$response_shape <- fit_pupil_response_shape_model(
      rspec, backend = backend_extra, chains = chains, iter = max(iter, 1600L), warmup = max(warmup, 800L),
      cores = cores, seed = seed + 40L, adapt_delta = 0.99, max_treedepth = 14L, refresh = 0L
    )
    print(estimate_pupil_response_parameters(extras$response_shape))
  }

  cat("\n============================================================\nREAL BACKEND SMOKE: COMPLETE\n============================================================\n")
  cat("No Git operation was performed. Inspect all sampler diagnostics before publication freeze.\n")
  invisible(list(core_fits = fits, backend_parity = parity, extras = extras))
}

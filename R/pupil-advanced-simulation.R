# Deterministic simulation for gp3bayes 0.5 advanced examples and validation.

.p05_student_noise <- function(n, sd, df) {
  stats::rt(n, df = df) * sd * sqrt((df - 2) / df)
}

.p05_ar_filter <- function(e, phi = numeric(), theta = numeric()) {
  n <- length(e)
  y <- numeric(n)
  p <- length(phi)
  q <- length(theta)
  for (i in seq_len(n)) {
    ar_part <- 0
    ma_part <- 0
    if (p) {
      for (j in seq_len(min(p, i - 1L))) ar_part <- ar_part + phi[[j]] * y[[i - j]]
    }
    if (q) {
      for (j in seq_len(min(q, i - 1L))) ma_part <- ma_part + theta[[j]] * e[[i - j]]
    }
    y[[i]] <- ar_part + e[[i]] + ma_part
  }
  y
}

#' Simulate advanced dynamic pupil time courses
#'
#' Generates deterministic hierarchical traces with optional Student-t
#' contamination, heteroskedasticity, ARMA dependence, measurement error, and
#' missingness. The simulator is intended for examples, recovery studies, and
#' failure-path validation; it does not claim physiological realism.
#'
#' @param n_participants Number of participants.
#' @param trials_per_participant Trials per participant.
#' @param time_points Number of samples per trial.
#' @param time_range Numeric length-two time range in milliseconds.
#' @param conditions Character condition labels.
#' @param family `"gaussian"` or `"student"`.
#' @param residual_scale Baseline residual SD.
#' @param heteroskedastic_strength Multiplicative time-varying noise strength.
#' @param ar Numeric AR coefficients, length at most 3.
#' @param ma Numeric MA coefficients, length at most 2.
#' @param participant_sd Participant random-intercept SD.
#' @param amplitude_condition Difference in response amplitude for condition 2.
#' @param latency_condition Difference in peak latency for condition 2.
#' @param outlier_fraction Fraction of observations receiving extra contamination.
#' @param missing_fraction Fraction of pupil observations set missing.
#' @param measurement_error_sd Known response-measurement SD; zero disables.
#' @param student_df Degrees of freedom when `family = "student"`.
#' @param seed Random seed.
#' @return A `gp3bayes_pupil_advanced_simulation` object with data and stored truth.
#' @export
simulate_advanced_pupil_timecourse <- function(
    n_participants = 24L,
    trials_per_participant = 6L,
    time_points = 41L,
    time_range = c(-500, 2500),
    conditions = c("control", "treatment"),
    family = c("gaussian", "student"),
    residual_scale = 0.08,
    heteroskedastic_strength = 0.35,
    ar = 0.45,
    ma = numeric(),
    participant_sd = 0.12,
    amplitude_condition = 0.22,
    latency_condition = 120,
    outlier_fraction = 0.01,
    missing_fraction = 0.03,
    measurement_error_sd = 0.015,
    student_df = 5,
    seed = 2026) {

  family <- match.arg(family)
  n_participants <- .p05_assert_integerish(n_participants, "n_participants", 2L, 10000L)
  trials_per_participant <- .p05_assert_integerish(trials_per_participant, "trials_per_participant", 1L, 10000L)
  time_points <- .p05_assert_integerish(time_points, "time_points", 8L, 100000L)
  if (length(time_range) != 2L || !all(is.finite(time_range)) || time_range[[1L]] >= time_range[[2L]]) stop("`time_range` must be increasing and finite.", call. = FALSE)
  if (length(conditions) < 1L || any(!nzchar(conditions))) stop("`conditions` must contain non-empty labels.", call. = FALSE)
  if (length(ar) > 3L || length(ma) > 2L || any(abs(c(ar, ma)) >= 0.98)) stop("Use at most AR(3)/MA(2) with coefficients strictly inside (-0.98, 0.98).", call. = FALSE)
  for (z in c(residual_scale, participant_sd, measurement_error_sd)) if (!is.finite(z) || z < 0) stop("Scale parameters must be finite and non-negative.", call. = FALSE)
  .p05_assert_fraction(outlier_fraction, "outlier_fraction")
  .p05_assert_fraction(missing_fraction, "missing_fraction")
  if (family == "student" && (!is.finite(student_df) || student_df <= 2)) stop("`student_df` must exceed 2.", call. = FALSE)

  set.seed(seed)
  times <- seq(time_range[[1L]], time_range[[2L]], length.out = time_points)
  participants <- sprintf("p%03d", seq_len(n_participants))
  trial_ids <- seq_len(trials_per_participant)
  base <- expand.grid(
    participant_id = participants,
    trial_id = trial_ids,
    time_ms = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base$condition <- conditions[((base$trial_id - 1L) %% length(conditions)) + 1L]
  base$condition <- factor(base$condition, levels = conditions)

  p_intercept <- stats::rnorm(n_participants, 0, participant_sd)
  names(p_intercept) <- participants
  trial_offset <- stats::rnorm(n_participants * trials_per_participant, 0, 0.035)
  names(trial_offset) <- as.vector(outer(participants, trial_ids, paste, sep = ":"))

  # Smooth response-generating curve: baseline + asymmetric bump + slow drift.
  cond_index <- as.integer(base$condition) - 1L
  amp <- 0.45 + amplitude_condition * cond_index
  latency <- 850 + latency_condition * cond_index
  rise <- 260
  decay <- 520
  dt <- base$time_ms
  left <- stats::plogis((dt - (latency - 250)) / rise)
  right <- stats::plogis(((latency + 650) - dt) / decay)
  response_shape <- amp * left * right
  drift <- 0.000025 * pmax(dt, 0)
  mu <- 3.2 + p_intercept[base$participant_id] + trial_offset[paste(base$participant_id, base$trial_id, sep = ":")] + response_shape + drift

  # Time-varying residual scale, deliberately strongest around the response.
  phase <- (base$time_ms - min(times)) / diff(range(times))
  sigma <- residual_scale * exp(heteroskedastic_strength * (phase - 0.5))

  series <- interaction(base$participant_id, base$trial_id, drop = TRUE)
  noise <- numeric(nrow(base))
  for (lev in levels(series)) {
    ii <- which(series == lev)
    raw <- if (family == "gaussian") stats::rnorm(length(ii), 0, sigma[ii]) else .p05_student_noise(length(ii), sigma[ii], student_df)
    # AR filtering assumes slowly varying sigma is already represented in raw innovations.
    noise[ii] <- .p05_ar_filter(raw, ar, ma)
  }

  contaminated <- stats::runif(nrow(base)) < outlier_fraction
  if (any(contaminated)) noise[contaminated] <- noise[contaminated] + stats::rnorm(sum(contaminated), 0, 5 * residual_scale)

  latent_pupil <- mu + noise
  response_se <- rep(measurement_error_sd, nrow(base))
  observed_pupil <- latent_pupil + if (measurement_error_sd > 0) stats::rnorm(nrow(base), 0, measurement_error_sd) else 0

  # Covariates with known uncertainty.
  participant_baseline <- 3.15 + p_intercept + stats::rnorm(n_participants, 0, 0.04)
  baseline_true <- participant_baseline[base$participant_id]
  baseline_se <- rep(0.03, nrow(base))
  baseline_observed <- baseline_true + stats::rnorm(nrow(base), 0, baseline_se)
  luminance_true <- 50 + 6 * sin(2 * pi * phase)
  luminance_se <- rep(1.5, nrow(base))
  luminance_observed <- luminance_true + stats::rnorm(nrow(base), 0, luminance_se)

  # Missingness probability varies gently with time to create a useful audit example.
  miss_prob <- pmin(0.8, missing_fraction * (0.6 + 0.8 * phase))
  missing <- stats::runif(nrow(base)) < miss_prob
  observed_pupil[missing] <- NA_real_

  data <- data.frame(
    participant_id = base$participant_id,
    trial_id = base$trial_id,
    condition = base$condition,
    time_ms = base$time_ms,
    pupil = observed_pupil,
    pupil_se = response_se,
    baseline_pupil = baseline_observed,
    baseline_se = baseline_se,
    luminance = luminance_observed,
    luminance_se = luminance_se,
    contaminated = contaminated,
    stringsAsFactors = FALSE
  )

  truth <- list(
    family = family,
    residual_scale = residual_scale,
    heteroskedastic_strength = heteroskedastic_strength,
    ar = ar,
    ma = ma,
    participant_sd = participant_sd,
    amplitude_condition = amplitude_condition,
    latency_condition = latency_condition,
    student_df = if (family == "student") student_df else NA_real_,
    mean = mu,
    sigma = sigma,
    latent_pupil = latent_pupil,
    missing = missing,
    seed = seed
  )

  structure(
    list(data = data, truth = truth),
    class = "gp3bayes_pupil_advanced_simulation"
  )
}

#' Simulate joint binocular pupil traces
#'
#' @param ... Arguments passed to [simulate_advanced_pupil_timecourse()].
#' @param residual_correlation Approximate left/right innovation correlation.
#' @param eye_bias Mean right-minus-left difference.
#' @param eye_specific_sd Eye-specific noise SD.
#' @return A `gp3bayes_binocular_pupil_simulation` object.
#' @export
simulate_binocular_pupil_timecourse <- function(
    ...,
    residual_correlation = 0.65,
    eye_bias = 0.015,
    eye_specific_sd = 0.035) {

  if (!is.finite(residual_correlation) || abs(residual_correlation) >= 0.99) stop("`residual_correlation` must lie inside (-0.99, 0.99).", call. = FALSE)
  sim <- simulate_advanced_pupil_timecourse(...)
  data <- sim$data
  latent <- sim$truth$latent_pupil
  n <- nrow(data)
  z1 <- stats::rnorm(n)
  z2 <- residual_correlation * z1 + sqrt(1 - residual_correlation^2) * stats::rnorm(n)
  left <- latent + eye_specific_sd * z1
  right <- latent + eye_bias + eye_specific_sd * z2
  miss <- is.na(data$pupil)
  # Eye-specific missingness creates realistic asymmetric availability without
  # silently averaging the eyes.
  left[miss & stats::runif(n) < 0.7] <- NA_real_
  right[miss & stats::runif(n) < 0.7] <- NA_real_
  out <- data
  out$pupil_left <- left
  out$pupil_right <- right
  out$pupil <- NULL
  structure(
    list(
      data = out,
      truth = c(sim$truth, list(residual_correlation = residual_correlation, eye_bias = eye_bias, eye_specific_sd = eye_specific_sd))
    ),
    class = "gp3bayes_binocular_pupil_simulation"
  )
}

#' Simulate data from the experimental nonlinear response-shape family
#'
#' @param n_participants,trials_per_participant,time_points Simulation sizes.
#' @param conditions Condition labels.
#' @param baseline,amplitude,onset,rise,duration,decay Shape parameters.
#' @param condition_amplitude_ratio Multiplicative amplitude ratio for condition 2.
#' @param condition_onset_shift Onset shift for condition 2.
#' @param residual_sd Residual SD.
#' @param seed Seed.
#' @export
simulate_pupil_response_shape <- function(
    n_participants = 20L,
    trials_per_participant = 6L,
    time_points = 41L,
    conditions = c("control", "treatment"),
    baseline = 3.2,
    amplitude = 0.7,
    onset = 250,
    rise = 180,
    duration = 1200,
    decay = 260,
    condition_amplitude_ratio = 1.2,
    condition_onset_shift = 80,
    residual_sd = 0.08,
    seed = 2026) {

  n_participants <- .p05_assert_integerish(n_participants, "n_participants", 2L, 10000L)
  trials_per_participant <- .p05_assert_integerish(trials_per_participant, "trials_per_participant", 1L, 10000L)
  time_points <- .p05_assert_integerish(time_points, "time_points", 8L, 100000L)
  if (!is.character(conditions) || length(conditions) < 1L || anyNA(conditions) || any(!nzchar(conditions))) stop("`conditions` must contain non-empty labels.", call. = FALSE)
  for (nm in c("baseline", "amplitude", "onset", "rise", "duration", "decay", "condition_amplitude_ratio", "condition_onset_shift", "residual_sd")) {
    z <- get(nm)
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z)) stop("Shape parameters must be finite numeric scalars.", call. = FALSE)
  }
  if (amplitude <= 0 || rise <= 0 || duration <= 0 || decay <= 0 || condition_amplitude_ratio <= 0 || residual_sd <= 0) {
    stop("`amplitude`, `rise`, `duration`, `decay`, `condition_amplitude_ratio`, and `residual_sd` must be positive.", call. = FALSE)
  }

  set.seed(seed)
  times <- seq(-500, 2500, length.out = time_points)
  d <- expand.grid(
    participant_id = sprintf("p%03d", seq_len(n_participants)),
    trial_id = seq_len(trials_per_participant),
    time_ms = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  d$condition <- factor(conditions[((d$trial_id - 1L) %% length(conditions)) + 1L], levels = conditions)
  cidx <- as.integer(d$condition) - 1L
  amp <- amplitude * condition_amplitude_ratio^cidx
  ons <- onset + condition_onset_shift * cidx
  participant_shift <- stats::rnorm(n_participants, 0, 0.1)
  names(participant_shift) <- unique(d$participant_id)
  shape <- stats::plogis((d$time_ms - ons) / rise) * stats::plogis((ons + duration - d$time_ms) / decay)
  mu <- baseline + participant_shift[d$participant_id] + amp * shape
  d$pupil <- mu + stats::rnorm(nrow(d), 0, residual_sd)
  structure(
    list(
      data = d,
      truth = list(
        baseline = baseline, amplitude = amplitude, onset = onset, rise = rise,
        duration = duration, decay = decay,
        condition_amplitude_ratio = condition_amplitude_ratio,
        condition_onset_shift = condition_onset_shift,
        residual_sd = residual_sd,
        mean = mu,
        seed = seed
      )
    ),
    class = "gp3bayes_pupil_response_shape_simulation"
  )
}

#' @export
print.gp3bayes_pupil_advanced_simulation <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_simulation>\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Participants:", length(unique(x$data$participant_id)), "\n")
  cat("  Family:", x$truth$family, "\n")
  cat("  Missing fraction:", format(mean(is.na(x$data$pupil)), digits = 3), "\n")
  invisible(x)
}

#' @export
print.gp3bayes_binocular_pupil_simulation <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_simulation>\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Residual correlation truth:", x$truth$residual_correlation, "\n")
  invisible(x)
}

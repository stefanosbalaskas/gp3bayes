.gp3p_waveform <- function(time, peak_latency = 0.9, shape = 3) {
  positive <- pmax(time, 0)
  scale <- peak_latency / shape
  value <- positive^shape * exp(-positive / scale)
  maximum <- (shape * scale)^shape * exp(-shape)
  if (!is.finite(maximum) || maximum <= 0) return(rep(0, length(time)))
  value / maximum
}

.gp3p_ar1_noise <- function(n, phi, sd) {
  if (n <= 0L) return(numeric())
  z <- stats::rnorm(n, sd = sd)
  if (n == 1L || abs(phi) < .Machine$double.eps) return(z)
  e <- numeric(n)
  e[1] <- z[1] / sqrt(max(1 - phi^2, .Machine$double.eps))
  for (i in 2:n) e[i] <- phi * e[i - 1L] + z[i]
  e
}

#' Simulate deterministic hierarchical pupil time courses
#'
#' Generates event-aligned synthetic pupil data with participant/item
#' heterogeneity, a smooth non-universal response waveform, AR(1) residual
#' dependence, optional blink/data-loss segments, gaze drift, and luminance
#' nuisance variation.
#'
#' @param n_participants Number of participants.
#' @param trials_per_participant Number of trials per participant.
#' @param n_items Number of crossed items; use `NULL` for no item effect.
#' @param sampling_frequency Sampling frequency in Hz.
#' @param time_window Two-element event-relative time window in seconds.
#' @param baseline_window Two-element pre-event baseline window.
#' @param conditions Character vector of condition labels.
#' @param baseline_pupil Baseline pupil level in millimetres.
#' @param response_amplitude Peak amplitude of the common simulated response.
#' @param condition_difference Additional peak amplitude in the second
#'   condition. For more than two conditions it is multiplied by the
#'   zero-based condition index.
#' @param peak_latency Time of the simulated waveform peak in seconds.
#' @param participant_sd,item_sd Standard deviations for simulated hierarchy.
#' @param residual_sd Innovation standard deviation.
#' @param ar1 AR(1) residual coefficient with absolute value below one.
#' @param blink_trial_probability Probability that a trial contains one
#'   synthetic blink/data-loss interval.
#' @param blink_duration Blink interval duration in seconds.
#' @param include_gaze,include_luminance Whether to add nuisance signals.
#' @param gaze_drift_sd Standard deviation of gaze drift increments.
#' @param luminance_amplitude Amplitude of the synthetic luminance nuisance.
#' @param seed Reproducibility seed.
#' @param max_rows Maximum allowed output rows.
#' @return A `gp3bayes_pupil_simulation` containing `data` and separate `truth`.
#' @section Interpretation:
#' The waveform is a convenient synthetic data-generating shape, not a claim
#' about a universal biological pupil response.
#' @examples
#' sim <- simulate_pupil_timecourse(
#'   n_participants = 4, trials_per_participant = 4,
#'   sampling_frequency = 20, seed = 2026
#' )
#' head(sim$data)
#' @export
simulate_pupil_timecourse <- function(
    n_participants = 20L,
    trials_per_participant = 12L,
    n_items = 12L,
    sampling_frequency = 60,
    time_window = c(-0.5, 2.5),
    baseline_window = c(-0.5, 0),
    conditions = c("control", "treatment"),
    baseline_pupil = 4.0,
    response_amplitude = 0.45,
    condition_difference = 0.18,
    peak_latency = 0.9,
    participant_sd = 0.25,
    item_sd = 0.08,
    residual_sd = 0.08,
    ar1 = 0.55,
    blink_trial_probability = 0.15,
    blink_duration = 0.12,
    include_gaze = TRUE,
    include_luminance = TRUE,
    gaze_drift_sd = 0.002,
    luminance_amplitude = 0.12,
    seed = 2026,
    max_rows = 500000L) {

  n_participants <- .gp3p_positive(n_participants, "n_participants", TRUE)
  trials_per_participant <- .gp3p_positive(trials_per_participant, "trials_per_participant", TRUE)
  if (!is.null(n_items)) n_items <- .gp3p_positive(n_items, "n_items", TRUE)
  sampling_frequency <- .gp3p_positive(sampling_frequency, "sampling_frequency")
  max_rows <- .gp3p_positive(max_rows, "max_rows", TRUE)
  for (nm in c("baseline_pupil", "response_amplitude", "peak_latency",
               "participant_sd", "item_sd", "residual_sd", "blink_duration",
               "gaze_drift_sd")) {
    value <- get(nm)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0) {
      .gp3p_stop("`", nm, "` must be one finite non-negative number.")
    }
  }
  if (!is.numeric(condition_difference) || length(condition_difference) != 1L ||
      is.na(condition_difference) || !is.finite(condition_difference)) {
    .gp3p_stop("`condition_difference` must be one finite number.")
  }
  if (!is.numeric(ar1) || length(ar1) != 1L || is.na(ar1) ||
      !is.finite(ar1) || abs(ar1) >= 1) {
    .gp3p_stop("`ar1` must be finite with absolute value below one.")
  }
  blink_trial_probability <- .gp3p_probability(
    blink_trial_probability, "blink_trial_probability"
  )
  include_gaze <- .gp3p_flag(include_gaze, "include_gaze")
  include_luminance <- .gp3p_flag(include_luminance, "include_luminance")
  conditions <- .gp3p_character_vector(conditions, "conditions")
  if (!length(conditions)) .gp3p_stop("`conditions` must contain at least one condition.")
  if (!is.numeric(time_window) || length(time_window) != 2L ||
      anyNA(time_window) || any(!is.finite(time_window)) ||
      time_window[1] >= time_window[2]) {
    .gp3p_stop("`time_window` must contain two finite increasing numbers.")
  }
  if (!is.numeric(baseline_window) || length(baseline_window) != 2L ||
      anyNA(baseline_window) || baseline_window[1] >= baseline_window[2] ||
      baseline_window[1] < time_window[1] || baseline_window[2] > time_window[2]) {
    .gp3p_stop("`baseline_window` must be an increasing interval inside `time_window`.")
  }

  dt <- 1 / sampling_frequency
  times <- seq(time_window[1], time_window[2] + dt / 10, by = dt)
  n_rows <- length(times) * n_participants * trials_per_participant
  if (n_rows > max_rows) {
    .gp3p_stop("Requested simulation would create ", n_rows,
               " rows, exceeding `max_rows = ", max_rows, "`.")
  }

  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) .gp3p_stop("`seed` must be one integer.")
  withr::with_seed(seed, {
    participants <- sprintf("P%03d", seq_len(n_participants))
    participant_effect <- stats::rnorm(n_participants, sd = participant_sd)
    names(participant_effect) <- participants
    if (!is.null(n_items)) {
      items <- sprintf("I%03d", seq_len(n_items))
      item_effect <- stats::rnorm(n_items, sd = item_sd)
      names(item_effect) <- items
    } else {
      items <- NA_character_
      item_effect <- 0
    }

    trial_index <- expand.grid(
      participant_id = participants,
      trial_number = seq_len(trials_per_participant),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    trial_index <- trial_index[order(trial_index$participant_id, trial_index$trial_number), ]
    trial_index$trial_id <- paste(trial_index$participant_id,
                                  sprintf("T%03d", trial_index$trial_number), sep = "_")
    trial_index$condition <- conditions[
      ((trial_index$trial_number - 1L) %% length(conditions)) + 1L
    ]
    if (!is.null(n_items)) {
      trial_index$item_id <- items[
        ((seq_len(nrow(trial_index)) - 1L) %% n_items) + 1L
      ]
    }

    rows <- vector("list", nrow(trial_index))
    for (j in seq_len(nrow(trial_index))) {
      ti <- trial_index[j, ]
      cond_index <- match(ti$condition, conditions) - 1L
      waveform <- .gp3p_waveform(times, peak_latency = peak_latency)
      luminance <- if (include_luminance) {
        0.5 + luminance_amplitude * sin(2 * pi * (times - min(times)) /
                                        max(diff(range(times)), dt) + j / 7)
      } else {
        rep(NA_real_, length(times))
      }
      nuisance <- if (include_luminance) 0.08 * (luminance - mean(luminance)) else 0
      ie <- if (!is.null(n_items)) item_effect[[ti$item_id]] else 0
      signal <- baseline_pupil +
        participant_effect[[ti$participant_id]] + ie +
        (response_amplitude + cond_index * condition_difference) * waveform +
        nuisance
      pupil <- signal + .gp3p_ar1_noise(length(times), ar1, residual_sd)

      blink <- rep(FALSE, length(times))
      if (stats::runif(1) < blink_trial_probability) {
        possible <- which(times >= 0.1 & times <= max(times) - blink_duration)
        if (length(possible)) {
          start_i <- sample(possible, 1L)
          blink <- times >= times[start_i] & times < times[start_i] + blink_duration
          pupil[blink] <- NA_real_
        }
      }

      if (include_gaze) {
        gx <- 0.5 + cumsum(stats::rnorm(length(times), sd = gaze_drift_sd))
        gy <- 0.5 + cumsum(stats::rnorm(length(times), sd = gaze_drift_sd))
      } else {
        gx <- gy <- rep(NA_real_, length(times))
      }

      rows[[j]] <- data.frame(
        participant_id = ti$participant_id,
        trial_id = ti$trial_id,
        item_id = if (!is.null(n_items)) ti$item_id else NA_character_,
        condition = ti$condition,
        event_time = times,
        timestamp = (j - 1) * (diff(time_window) + 1) + (times - min(times)),
        pupil_mm = pupil,
        pupil_signal_truth_mm = signal,
        blink = blink,
        interpolated = FALSE,
        valid = !blink,
        gaze_x = gx,
        gaze_y = gy,
        luminance = luminance,
        stringsAsFactors = FALSE
      )
    }
    data <- do.call(rbind, rows)
    rownames(data) <- NULL

    truth <- list(
      seed = seed,
      waveform = "normalized gamma-shaped synthetic response",
      sampling_frequency = sampling_frequency,
      time_window = time_window,
      baseline_window = baseline_window,
      baseline_pupil_mm = baseline_pupil,
      response_amplitude_mm = response_amplitude,
      condition_difference_mm = condition_difference,
      peak_latency_s = peak_latency,
      participant_sd_mm = participant_sd,
      item_sd_mm = if (is.null(n_items)) NA_real_ else item_sd,
      residual_innovation_sd_mm = residual_sd,
      ar1 = ar1,
      blink_trial_probability = blink_trial_probability,
      luminance_nuisance_included = include_luminance,
      gaze_drift_included = include_gaze,
      psychological_construct = NA_character_
    )

    structure(
      list(data = data, truth = truth),
      class = "gp3bayes_pupil_simulation"
    )
  })
}

#' @export
print.gp3bayes_pupil_simulation <- function(x, ...) {
  cat("<gp3bayes_pupil_simulation>\n")
  cat("  Rows: ", nrow(x$data), "\n", sep = "")
  cat("  Participants: ", length(unique(x$data$participant_id)), "\n", sep = "")
  cat("  Trials: ", length(unique(x$data$trial_id)), "\n", sep = "")
  cat("  Sampling frequency: ", x$truth$sampling_frequency, " Hz\n", sep = "")
  cat("  Synthetic truth stored separately: TRUE\n")
  invisible(x)
}

.gp3p_as_indicator <- function(x, name) {
  if (is.logical(x)) return(x)
  if (is.numeric(x) && all(is.na(x) | x %in% c(0, 1))) return(as.logical(x))
  .gp3p_stop("Declared `", name, "` must be logical or coded 0/1.")
}

.gp3p_convert_unit <- function(x, from, to) {
  if (identical(from, to)) return(x)
  if (identical(from, "metres") && identical(to, "millimetres")) return(x * 1000)
  if (identical(from, "millimetres") && identical(to, "metres")) return(x / 1000)
  .gp3p_stop("No deterministic physical conversion is approved from `",
             from, "` to `", to, "`.")
}

.gp3p_series_summary <- function(data) {
  split_dt <- split(data$.event_time, data$.series_id)
  intervals <- unlist(lapply(split_dt, function(t) diff(sort(unique(t)))), use.names = FALSE)
  intervals <- intervals[is.finite(intervals) & intervals > 0]
  if (!length(intervals)) {
    return(list(median_dt = NA_real_, cv_dt = NA_real_, estimated_hz = NA_real_))
  }
  median_dt <- stats::median(intervals)
  list(
    median_dt = median_dt,
    cv_dt = if (mean(intervals) == 0) NA_real_ else stats::sd(intervals) / mean(intervals),
    estimated_hz = 1 / median_dt
  )
}

#' Prepare a pupil time course under explicit transformations
#'
#' Validates ordering, identifiers, event time, pupil values, measurement flags,
#' sampling intervals, and baseline support. Only explicitly requested
#' deterministic transformations are applied and recorded.
#'
#' @param data Source data frame.
#' @param contract A `create_pupil_contract()` result.
#' @param baseline_operation One of `"none"`, `"subtract"`, `"divide"`,
#'   `"proportion_change"`, or `"percent_change"`.
#' @param baseline_window Required when `baseline_operation != "none"` unless
#'   already recorded in the contract. Values use the contract's source
#'   `time_unit` and are converted to canonical seconds during preparation.
#' @param output_unit Optional physical output unit. Only metre/millimetre
#'   conversions are defined.
#' @param scale_covariates Declared numeric covariates to standardize.
#' @param max_rows Maximum accepted input rows.
#' @param irregularity_review_cv Coefficient-of-variation threshold recorded
#'   as a sampling-irregularity review signal.
#' @return A `gp3bayes_pupil_prepared` object. The source pupil values are
#'   retained in `.pupil_source`; the modelled values are `.pupil_model`.
#' @section Governance boundary:
#' This function does not detect or interpolate blinks, smooth traces, choose a
#' baseline, correct PFE, correct luminance, or automatically exclude samples.
#' @examples
#' sim <- simulate_pupil_timecourse(
#'   n_participants = 3, trials_per_participant = 3,
#'   sampling_frequency = 20, seed = 11
#' )
#' contract <- create_pupil_contract(
#'   "pupil_mm", "participant_id", "trial_id", "event_time",
#'   "millimetres", 20, condition_col = "condition",
#'   blink_col = "blink", interpolation_col = "interpolated",
#'   validity_col = "valid", gaze_x_col = "gaze_x", gaze_y_col = "gaze_y",
#'   luminance_col = "luminance", baseline_window = c(-0.5, 0)
#' )
#' prepared <- prepare_pupil_timecourse(
#'   sim$data, contract, baseline_operation = "subtract"
#' )
#' prepared
#' @export
prepare_pupil_timecourse <- function(
    data,
    contract,
    baseline_operation = c("none", "subtract", "divide",
                           "proportion_change", "percent_change"),
    baseline_window = NULL,
    output_unit = NULL,
    scale_covariates = character(),
    max_rows = 2000000L,
    irregularity_review_cv = 0.10) {

  .gp3p_validate_contract(contract)
  if (!is.data.frame(data) || !nrow(data)) .gp3p_stop("`data` must be a non-empty data frame.")
  max_rows <- .gp3p_positive(max_rows, "max_rows", TRUE)
  if (nrow(data) > max_rows) {
    .gp3p_stop("Input contains ", nrow(data), " rows, exceeding `max_rows = ",
               max_rows, "`.")
  }
  baseline_operation <- match.arg(baseline_operation)
  scale_covariates <- .gp3p_character_vector(scale_covariates, "scale_covariates")
  if (!is.numeric(irregularity_review_cv) || length(irregularity_review_cv) != 1L ||
      is.na(irregularity_review_cv) || !is.finite(irregularity_review_cv) ||
      irregularity_review_cv < 0) {
    .gp3p_stop("`irregularity_review_cv` must be one finite non-negative number.")
  }

  mappings <- contract$mappings
  required <- c(mappings$outcome, mappings$participant, mappings$trial, mappings$time)
  optional <- unlist(mappings[setdiff(names(mappings), c("outcome","participant","trial","time"))],
                     use.names = FALSE)
  channel_audit_columns <- c(
    contract$measurement$left_pupil_col,
    contract$measurement$right_pupil_col
  )
  .gp3p_check_columns(
    data,
    c(required, optional, channel_audit_columns, scale_covariates)
  )

  pupil <- data[[mappings$outcome]]
  time <- data[[mappings$time]]
  if (!is.numeric(pupil)) .gp3p_stop("The declared pupil outcome must be numeric.")
  if (!is.numeric(time) || anyNA(time) || any(!is.finite(time))) {
    .gp3p_stop("The declared event-relative time must be numeric, finite, and non-missing.")
  }
  nonmissing_pupil <- pupil[!is.na(pupil)]
  if (any(!is.finite(nonmissing_pupil))) {
    .gp3p_stop("Non-missing pupil values must be finite.")
  }

  participant <- data[[mappings$participant]]
  trial <- data[[mappings$trial]]
  if (anyNA(participant) || anyNA(trial)) {
    .gp3p_stop("Participant and trial identifiers must not be missing.")
  }
  if (!length(unique(participant))) .gp3p_stop("No participants were found.")
  if (!length(unique(trial))) .gp3p_stop("No trials were found.")

  if (!is.null(mappings$item) && anyNA(data[[mappings$item]])) {
    .gp3p_stop("The declared item identifier must not contain missing values.")
  }
  if (!is.null(mappings$condition) && anyNA(data[[mappings$condition]])) {
    .gp3p_stop("The declared condition must not contain missing values.")
  }
  for (nm in c("left_pupil_col", "right_pupil_col")) {
    mapped <- contract$measurement[[nm]]
    if (!is.null(mapped) && !is.numeric(data[[mapped]])) {
      .gp3p_stop("Declared paired pupil audit column `", mapped, "` must be numeric.")
    }
    if (!is.null(mapped)) {
      observed <- data[[mapped]][!is.na(data[[mapped]])]
      if (any(!is.finite(observed))) {
        .gp3p_stop("Non-missing values in paired pupil audit column `", mapped,
                   "` must be finite.")
      }
    }
  }
  numeric_context <- c("gaze_x", "gaze_y", "luminance", "contrast")
  for (nm in numeric_context) {
    mapped <- mappings[[nm]]
    if (!is.null(mapped) && !is.numeric(data[[mapped]])) {
      .gp3p_stop("Declared `", nm, "` column `", mapped, "` must be numeric.")
    }
    if (!is.null(mapped)) {
      observed <- data[[mapped]][!is.na(data[[mapped]])]
      if (any(!is.finite(observed))) {
        .gp3p_stop("Non-missing values in declared `", nm, "` must be finite.")
      }
    }
  }
  if (contract$pupil_unit %in% c("millimetres", "metres", "pixels") &&
      !isTRUE(contract$preprocessing$baseline_applied) &&
      any(nonmissing_pupil <= 0)) {
    .gp3p_stop(
      "Unadjusted pupil values in physical/pixel units must be strictly positive. ",
      "If the supplied series is already baseline-adjusted, declare that in the contract."
    )
  }

  out <- data
  out$.source_row <- seq_len(nrow(out))
  out$.participant <- factor(participant)
  out$.trial <- factor(trial)
  out$.series_id <- interaction(out$.participant, out$.trial, drop = TRUE, lex.order = TRUE)
  out$.event_time_source <- as.numeric(time)
  out$.event_time <- if (identical(contract$time_unit, "milliseconds")) {
    out$.event_time_source / 1000
  } else {
    out$.event_time_source
  }
  out$.pupil_source <- as.numeric(pupil)
  out$.pupil_model <- out$.pupil_source
  out$.sample_index <- stats::ave(out$.event_time, out$.series_id,
                           FUN = function(z) rank(z, ties.method = "first"))

  if (!is.null(mappings$item)) out$.item <- factor(out[[mappings$item]])
  if (!is.null(mappings$condition)) out$.condition <- factor(out[[mappings$condition]])
  if (!is.null(mappings$timestamp)) out$.timestamp <- out[[mappings$timestamp]]
  if (!is.null(mappings$validity)) {
    out$.valid <- .gp3p_as_indicator(out[[mappings$validity]], "validity_col")
  } else out$.valid <- NA
  if (!is.null(mappings$interpolated)) {
    out$.interpolated <- .gp3p_as_indicator(out[[mappings$interpolated]], "interpolation_col")
  } else out$.interpolated <- NA
  if (!is.null(mappings$blink)) {
    out$.blink <- .gp3p_as_indicator(out[[mappings$blink]], "blink_col")
  } else out$.blink <- NA
  if (!is.null(mappings$gaze_x)) out$.gaze_x <- as.numeric(out[[mappings$gaze_x]])
  if (!is.null(mappings$gaze_y)) out$.gaze_y <- as.numeric(out[[mappings$gaze_y]])
  if (!is.null(mappings$luminance)) out$.luminance <- as.numeric(out[[mappings$luminance]])
  if (!is.null(mappings$contrast)) out$.contrast <- as.numeric(out[[mappings$contrast]])
  if (!is.null(contract$measurement$left_pupil_col)) {
    out$.pupil_left_audit <- as.numeric(out[[contract$measurement$left_pupil_col]])
  }
  if (!is.null(contract$measurement$right_pupil_col)) {
    out$.pupil_right_audit <- as.numeric(out[[contract$measurement$right_pupil_col]])
  }

  key <- paste(out$.participant, out$.trial, format(out$.event_time, digits = 17), sep = "\r")
  if (anyDuplicated(key)) {
    .gp3p_stop("Duplicated participant-trial-time samples were detected; preparation does not repair them.")
  }

  ord <- order(out$.participant, out$.trial, out$.event_time, out$.source_row)
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out$.sample_index <- stats::ave(out$.event_time, out$.series_id, FUN = seq_along)

  source_unit <- contract$pupil_unit
  model_unit <- source_unit
  transformations <- list()
  if (identical(contract$time_unit, "milliseconds")) {
    transformations[[length(transformations) + 1L]] <- list(
      operation = "time_unit_conversion",
      from = "milliseconds",
      to = "seconds",
      factor = 0.001
    )
  }
  if (!is.null(output_unit)) {
    output_unit <- .gp3p_match_unit(output_unit)
    out$.pupil_model <- .gp3p_convert_unit(out$.pupil_model, source_unit, output_unit)
    transformations[[length(transformations) + 1L]] <- list(
      operation = "unit_conversion", from = source_unit, to = output_unit
    )
    model_unit <- output_unit
  }

  chosen_baseline_window <- .gp3p_null_or(
    baseline_window,
    contract$preprocessing$baseline_window
  )
  if (!is.null(chosen_baseline_window)) {
    if (!is.numeric(chosen_baseline_window) || length(chosen_baseline_window) != 2L ||
        anyNA(chosen_baseline_window) || any(!is.finite(chosen_baseline_window)) ||
        chosen_baseline_window[1] >= chosen_baseline_window[2]) {
      .gp3p_stop("`baseline_window` must contain two finite increasing values.")
    }
    if (identical(contract$time_unit, "milliseconds")) {
      chosen_baseline_window <- as.numeric(chosen_baseline_window) / 1000
    } else {
      chosen_baseline_window <- as.numeric(chosen_baseline_window)
    }
  }
  baseline_values <- NULL
  if (!identical(baseline_operation, "none")) {
    if (isTRUE(contract$preprocessing$baseline_applied)) {
      .gp3p_stop("The contract declares baseline correction already applied; a second baseline operation is blocked.")
    }
    if (is.null(chosen_baseline_window) || !is.numeric(chosen_baseline_window) ||
        length(chosen_baseline_window) != 2L || anyNA(chosen_baseline_window) ||
        chosen_baseline_window[1] >= chosen_baseline_window[2]) {
      .gp3p_stop("A valid two-element `baseline_window` is required for baseline transformation.")
    }
    in_baseline <- out$.event_time >= chosen_baseline_window[1] &
      out$.event_time <= chosen_baseline_window[2]
    baseline_values <- tapply(out$.pupil_model[in_baseline],
                              out$.series_id[in_baseline], mean, na.rm = TRUE)
    baseline_values[!is.finite(baseline_values)] <- NA_real_
    missing_series <- setdiff(levels(out$.series_id), names(baseline_values)[!is.na(baseline_values)])
    if (length(missing_series)) {
      .gp3p_stop("Baseline transformation blocked: ", length(missing_series),
                 " trial series lack a finite baseline estimate.")
    }
    b <- as.numeric(baseline_values[as.character(out$.series_id)])
    if (baseline_operation %in% c("divide", "proportion_change", "percent_change") &&
        any(b == 0, na.rm = TRUE)) {
      .gp3p_stop("Baseline division is undefined because at least one baseline mean is zero.")
    }
    if (identical(baseline_operation, "subtract")) out$.pupil_model <- out$.pupil_model - b
    if (identical(baseline_operation, "divide")) {
      out$.pupil_model <- out$.pupil_model / b
      model_unit <- "ratio"
    }
    if (identical(baseline_operation, "proportion_change")) {
      out$.pupil_model <- (out$.pupil_model - b) / b
      model_unit <- "proportion_change"
    }
    if (identical(baseline_operation, "percent_change")) {
      out$.pupil_model <- 100 * (out$.pupil_model - b) / b
      model_unit <- "percent_change"
    }
    transformations[[length(transformations) + 1L]] <- list(
      operation = "baseline",
      method = baseline_operation,
      window = as.numeric(chosen_baseline_window)
    )
  }

  scaling <- list()
  for (nm in scale_covariates) {
    if (!is.numeric(out[[nm]])) .gp3p_stop("Scaled covariate `", nm, "` must be numeric.")
    center <- mean(out[[nm]], na.rm = TRUE)
    scale <- stats::sd(out[[nm]], na.rm = TRUE)
    if (!is.finite(center) || !is.finite(scale) || scale <= 0) {
      .gp3p_stop("Scaled covariate `", nm, "` must have finite non-zero variation.")
    }
    new_nm <- paste0(".z_", make.names(nm))
    out[[new_nm]] <- (out[[nm]] - center) / scale
    scaling[[nm]] <- list(output = new_nm, center = center, scale = scale)
  }
  if (length(scaling)) {
    transformations[[length(transformations) + 1L]] <- list(
      operation = "covariate_standardization", details = scaling
    )
  }

  timing <- .gp3p_series_summary(out)
  timing$declared_hz <- contract$sampling_frequency
  timing$relative_frequency_error <- if (is.finite(timing$estimated_hz)) {
    abs(timing$estimated_hz - contract$sampling_frequency) / contract$sampling_frequency
  } else NA_real_
  timing$irregularity_review_cv <- irregularity_review_cv

  result <- structure(
    list(
      preparation_version = "0.4-pupil-1",
      family = "pupil",
      data = out,
      contract = contract,
      source_unit = source_unit,
      model_unit = model_unit,
      source_time_unit = contract$time_unit,
      model_time_unit = "seconds",
      baseline_operation = baseline_operation,
      baseline_window = chosen_baseline_window,
      baseline_values = baseline_values,
      transformations = transformations,
      scaling = scaling,
      timing = timing,
      row_accounting = data.frame(
        stage = c("source", "prepared", "nonmissing_model_outcome"),
        rows = c(nrow(data), nrow(out), sum(!is.na(out$.pupil_model))),
        stringsAsFactors = FALSE
      ),
      fit_performed = FALSE
    ),
    class = c("gp3bayes_pupil_prepared", "gp3bayes_prepared")
  )
  result$audit <- audit_pupil_readiness(result)
  result
}

.gp3p_metric_row <- function(metric, value, status = "pass", detail = "") {
  data.frame(metric = metric, value = as.character(value), status = status,
             detail = detail, stringsAsFactors = FALSE)
}

.gp3p_prop <- function(x, truth = TRUE) {
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x == truth, na.rm = TRUE)
}

#' Audit pupil-timecourse readiness
#'
#' Produces observable evidence about hierarchy, sampling, missingness,
#' baseline support, measurement flags, gaze/PFE context, luminance, and
#' preprocessing provenance. Review signals are not exclusion decisions.
#'
#' @param x A `gp3bayes_pupil_prepared` object or raw data with `contract`.
#' @param contract Required only when `x` is raw data.
#' @return A `gp3bayes_pupil_readiness` object with summary and stratified
#'   tables.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
audit_pupil_readiness <- function(x, contract = NULL) {
  if (!inherits(x, "gp3bayes_pupil_prepared")) {
    if (is.null(contract)) .gp3p_stop("Supply `contract` when auditing raw data.")
    x <- prepare_pupil_timecourse(x, contract, baseline_operation = "none")
    return(x$audit)
  }
  d <- x$data
  contract <- x$contract
  timing <- x$timing
  missing_prop <- mean(is.na(d$.pupil_model))
  pupil_range <- range(d$.pupil_model, na.rm = TRUE)
  if (any(!is.finite(pupil_range))) pupil_range <- c(NA_real_, NA_real_)
  n_conditions <- if (".condition" %in% names(d)) nlevels(d$.condition) else 0L
  n_items <- if (".item" %in% names(d)) nlevels(d$.item) else 0L

  baseline_window <- .gp3p_null_or(x$baseline_window, contract$preprocessing$baseline_window)
  baseline_coverage <- NA_real_
  trials_lacking_baseline <- NA_integer_
  if (!is.null(baseline_window)) {
    in_b <- d$.event_time >= baseline_window[1] & d$.event_time <= baseline_window[2]
    by_series <- tapply(!is.na(d$.pupil_model) & in_b, d$.series_id, any)
    baseline_coverage <- mean(by_series)
    trials_lacking_baseline <- sum(!by_series)
  }

  blink_prop <- if (".blink" %in% names(d)) .gp3p_prop(d$.blink) else NA_real_
  interp_prop <- if (".interpolated" %in% names(d)) .gp3p_prop(d$.interpolated) else NA_real_
  invalid_prop <- if (".valid" %in% names(d)) .gp3p_prop(d$.valid, FALSE) else NA_real_

  gaze_available <- all(c(".gaze_x", ".gaze_y") %in% names(d)) &&
    any(is.finite(d$.gaze_x)) && any(is.finite(d$.gaze_y))
  luminance_available <- ".luminance" %in% names(d) && any(is.finite(d$.luminance))
  contrast_available <- ".contrast" %in% names(d) && any(is.finite(d$.contrast))

  gaze_condition_imbalance <- NA_real_
  luminance_condition_imbalance <- NA_real_
  by_condition <- if (".condition" %in% names(d)) {
    do.call(rbind, lapply(split(d, d$.condition), function(z) {
      data.frame(
        condition = as.character(z$.condition[1]),
        rows = nrow(z),
        participants = length(unique(z$.participant)),
        missing_pupil_proportion = mean(is.na(z$.pupil_model)),
        gaze_x_mean = if (".gaze_x" %in% names(z)) mean(z$.gaze_x, na.rm = TRUE) else NA_real_,
        gaze_y_mean = if (".gaze_y" %in% names(z)) mean(z$.gaze_y, na.rm = TRUE) else NA_real_,
        luminance_mean = if (".luminance" %in% names(z)) mean(z$.luminance, na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame()
  }
  if (nrow(by_condition) >= 2L) {
    if (all(is.finite(by_condition$gaze_x_mean)) &&
        all(is.finite(by_condition$gaze_y_mean))) {
      gaze_condition_imbalance <- max(sqrt(
        (by_condition$gaze_x_mean - mean(by_condition$gaze_x_mean))^2 +
          (by_condition$gaze_y_mean - mean(by_condition$gaze_y_mean))^2
      ))
    }
    if (all(is.finite(by_condition$luminance_mean))) {
      luminance_condition_imbalance <- diff(range(by_condition$luminance_mean))
    }
  }

  eye_disagreement <- NA_real_
  eye_disagreement_unit <- NA_character_
  if (all(c(".pupil_left_audit", ".pupil_right_audit") %in% names(d))) {
    both <- is.finite(d$.pupil_left_audit) & is.finite(d$.pupil_right_audit)
    if (any(both)) {
      eye_disagreement <- stats::median(
        abs(d$.pupil_left_audit[both] - d$.pupil_right_audit[both])
      )
      eye_disagreement_unit <- contract$measurement$channel_audit_unit
    }
  } else if (all(c("LPD", "RPD") %in% names(d))) {
    both <- is.finite(d$LPD) & is.finite(d$RPD)
    if (any(both)) {
      eye_disagreement <- stats::median(abs(d$LPD[both] - d$RPD[both]))
      eye_disagreement_unit <- "pixels"
    }
  } else if (all(c("LPUPILD", "RPUPILD") %in% names(d))) {
    both <- is.finite(d$LPUPILD) & is.finite(d$RPUPILD)
    if (any(both)) {
      eye_disagreement <- stats::median(abs(d$LPUPILD[both] - d$RPUPILD[both]))
      eye_disagreement_unit <- "metres"
    }
  }

  by_trial <- do.call(rbind, lapply(split(d, d$.series_id), function(z) {
    data.frame(
      series_id = as.character(z$.series_id[1]),
      participant = as.character(z$.participant[1]),
      trial = as.character(z$.trial[1]),
      time_start = min(z$.event_time),
      time_end = max(z$.event_time),
      time_span = diff(range(z$.event_time)),
      rows = nrow(z),
      nonmissing_pupil = sum(!is.na(z$.pupil_model)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_trial) <- NULL

  provenance_fields <- c(
    contract$preprocessing$provenance,
    contract$preprocessing$upstream_package,
    contract$preprocessing$upstream_version
  )
  provenance_complete <- mean(!is.na(provenance_fields) & nzchar(provenance_fields))

  rows <- rbind(
    .gp3p_metric_row("rows", nrow(d)),
    .gp3p_metric_row("participants", nlevels(d$.participant),
                     if (nlevels(d$.participant) >= 2L) "pass" else "review",
                     "One-participant data cannot support population participant heterogeneity."),
    .gp3p_metric_row("trials", nlevels(d$.trial)),
    .gp3p_metric_row("items", n_items, if (n_items) "pass" else "review",
                     "Item hierarchy is optional."),
    .gp3p_metric_row("conditions", n_conditions, if (n_conditions >= 2L) "pass" else "review",
                     "A condition contrast requires at least two levels."),
    .gp3p_metric_row("estimated_sampling_hz", signif(timing$estimated_hz, 6),
                     if (is.finite(timing$relative_frequency_error) &&
                         timing$relative_frequency_error <= .10) "pass" else "review",
                     paste0("Declared ", contract$sampling_frequency, " Hz.")),
    .gp3p_metric_row("median_sampling_interval", signif(timing$median_dt, 6)),
    .gp3p_metric_row("sampling_interval_cv", signif(timing$cv_dt, 6),
                     if (is.finite(timing$cv_dt) &&
                         timing$cv_dt <= timing$irregularity_review_cv) "pass" else "review",
                     "AR(1) sample-order dependence requires regular-enough sampling."),
    .gp3p_metric_row("missing_pupil_proportion", signif(missing_prop, 6),
                     if (missing_prop <= .10) "pass" else "review",
                     "Missingness is reported, not automatically repaired or excluded."),
    .gp3p_metric_row("blink_proportion", signif(blink_prop, 6),
                     if (is.na(blink_prop)) "review" else "pass"),
    .gp3p_metric_row("interpolated_proportion", signif(interp_prop, 6),
                     if (is.na(interp_prop)) "review" else "pass"),
    .gp3p_metric_row("invalid_proportion", signif(invalid_prop, 6),
                     if (is.na(invalid_prop)) "review" else "pass"),
    .gp3p_metric_row("baseline_coverage", signif(baseline_coverage, 6),
                     if (is.na(baseline_coverage)) "review"
                     else if (baseline_coverage == 1) "pass" else "review"),
    .gp3p_metric_row("trials_lacking_baseline", trials_lacking_baseline,
                     if (is.na(trials_lacking_baseline)) "review"
                     else if (trials_lacking_baseline == 0) "pass" else "review"),
    .gp3p_metric_row("pupil_min", signif(pupil_range[1], 6)),
    .gp3p_metric_row("pupil_max", signif(pupil_range[2], 6)),
    .gp3p_metric_row("minimum_trial_time_span",
                     signif(min(by_trial$time_span, na.rm = TRUE), 6),
                     "review", "Smallest observed within-trial event-time coverage."),
    .gp3p_metric_row("maximum_trial_time_span",
                     signif(max(by_trial$time_span, na.rm = TRUE), 6),
                     "review", "Largest observed within-trial event-time coverage."),
    .gp3p_metric_row("gaze_available", gaze_available,
                     if (gaze_available) "pass" else "review"),
    .gp3p_metric_row("gaze_condition_imbalance", signif(gaze_condition_imbalance, 6),
                     "review", "Descriptive between-condition gaze-position difference."),
    .gp3p_metric_row("left_right_pupil_disagreement", signif(eye_disagreement, 6),
                     "review", paste0("Median absolute paired-channel difference",
                                      if (!is.na(eye_disagreement_unit))
                                        paste0(" (", eye_disagreement_unit, ")") else ".")),
    .gp3p_metric_row("luminance_available", luminance_available,
                     if (luminance_available) "pass" else "review"),
    .gp3p_metric_row("luminance_condition_imbalance", signif(luminance_condition_imbalance, 6),
                     "review", "Descriptive between-condition mean luminance difference."),
    .gp3p_metric_row("contrast_available", contrast_available,
                     if (contrast_available) "pass" else "review"),
    .gp3p_metric_row("pfe_corrected_upstream", contract$preprocessing$pfe_corrected,
                     "review", "PFE status is contextual evidence, not an automatic correction."),
    .gp3p_metric_row("preprocessing_provenance_completeness",
                     signif(provenance_complete, 3),
                     if (provenance_complete == 1) "pass" else "review")
  )

  by_participant <- do.call(rbind, lapply(split(d, d$.participant), function(z) {
    data.frame(
      participant = as.character(z$.participant[1]),
      rows = nrow(z),
      trials = length(unique(z$.trial)),
      missing_pupil_proportion = mean(is.na(z$.pupil_model)),
      blink_proportion = if (".blink" %in% names(z)) .gp3p_prop(z$.blink) else NA_real_,
      interpolated_proportion = if (".interpolated" %in% names(z)) .gp3p_prop(z$.interpolated) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_participant) <- NULL

  if (nrow(by_condition)) rownames(by_condition) <- NULL

  status <- if (any(rows$status == "fail")) "fail"
  else if (any(rows$status == "review")) "review" else "pass"

  structure(
    list(
      audit_version = "0.4-pupil-1",
      family = "pupil",
      status = status,
      summary = rows,
      by_participant = by_participant,
      by_condition = by_condition,
      by_trial = by_trial,
      evidence_only = TRUE,
      automatic_exclusion = FALSE
    ),
    class = c("gp3bayes_pupil_readiness", "gp3bayes_readiness_audit")
  )
}

#' Extract pupil readiness tables
#' @param x A `gp3bayes_pupil_readiness`.
#' @param component One of `"summary"`, `"participant"`, `"condition"`, or `"trial"`.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_readiness_table <- function(x, component = c("summary", "participant", "condition", "trial")) {
  if (!inherits(x, "gp3bayes_pupil_readiness")) {
    .gp3p_stop("`x` must be a pupil readiness audit.")
  }
  component <- match.arg(component)
  switch(component,
         summary = x$summary,
         participant = x$by_participant,
         condition = x$by_condition,
         trial = x$by_trial)
}

#' Audit pupil measurement and confound context
#'
#' Summarises declared blink/interpolation, baseline, PFE/gaze, luminance,
#' contrast, and sampling context without correcting or excluding observations.
#' @param x A prepared pupil object.
#' @return A `gp3bayes_pupil_measurement_audit`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
audit_pupil_measurement_context <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_prepared")) {
    .gp3p_stop("`x` must be created by `prepare_pupil_timecourse()`.")
  }
  d <- x$data
  cnd <- x$audit$by_condition
  range_string <- function(v) {
    v <- v[is.finite(v)]
    if (!length(v)) return(NA_character_)
    paste(signif(range(v), 5), collapse = " to ")
  }
  finite_span <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 2L) return(NA_real_)
    diff(range(v))
  }
  gaze_imbalance <- if (nrow(cnd) >= 2L &&
                        all(c("gaze_x_mean", "gaze_y_mean") %in% names(cnd))) {
    max(finite_span(cnd$gaze_x_mean), finite_span(cnd$gaze_y_mean), na.rm = TRUE)
  } else NA_real_
  if (!is.finite(gaze_imbalance)) gaze_imbalance <- NA_real_
  luminance_imbalance <- if (nrow(cnd) >= 2L && "luminance_mean" %in% names(cnd)) {
    finite_span(cnd$luminance_mean)
  } else NA_real_

  table <- data.frame(
    domain = c(
      "blink", "interpolation", "baseline", "sampling", "gaze_position",
      "pfe", "luminance", "contrast", "time_on_task"
    ),
    available = c(
      !is.null(x$contract$mappings$blink),
      !is.null(x$contract$mappings$interpolated),
      !is.null(x$baseline_window), is.finite(x$timing$median_dt),
      all(c(".gaze_x", ".gaze_y") %in% names(d)),
      TRUE, ".luminance" %in% names(d), ".contrast" %in% names(d),
      ".timestamp" %in% names(d)
    ),
    observed = c(
      if (".blink" %in% names(d)) signif(.gp3p_prop(d$.blink), 5) else NA,
      if (".interpolated" %in% names(d)) signif(.gp3p_prop(d$.interpolated), 5) else NA,
      if (is.null(x$baseline_window)) NA else paste(x$baseline_window, collapse = " to "),
      signif(x$timing$cv_dt, 5),
      signif(gaze_imbalance, 5),
      as.character(x$contract$preprocessing$pfe_corrected),
      signif(luminance_imbalance, 5),
      if (".contrast" %in% names(d)) range_string(d$.contrast) else NA,
      if (".timestamp" %in% names(d)) range_string(as.numeric(d$.timestamp)) else NA
    ),
    interpretation = c(
      "Reported data-loss context only.",
      "Reported upstream interpolation context only.",
      "Baseline declaration; no automatic choice.",
      "Sampling irregularity context for temporal modelling.",
      "Between-condition mean gaze-position difference; descriptive only.",
      "Upstream PFE-correction declaration; no automatic PFE correction.",
      "Between-condition mean luminance difference; descriptive only.",
      "Contrast availability/range; no automatic adjustment.",
      "Recording-time support when available."
    ),
    stringsAsFactors = FALSE
  )
  structure(
    list(table = table, by_condition = cnd, evidence_only = TRUE),
    class = "gp3bayes_pupil_measurement_audit"
  )
}

#' Extract a pupil measurement-context table
#' @param x A `gp3bayes_pupil_measurement_audit`.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_measurement_audit_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_measurement_audit")) {
    .gp3p_stop("`x` must be a pupil measurement audit.")
  }
  x$table
}

#' @export
print.gp3bayes_pupil_prepared <- function(x, ...) {
  cat("<gp3bayes_pupil_prepared>\n")
  cat("  Rows: ", nrow(x$data), "\n", sep = "")
  cat("  Participants: ", nlevels(x$data$.participant), "\n", sep = "")
  cat("  Trials: ", nlevels(x$data$.trial), "\n", sep = "")
  cat("  Model unit: ", x$model_unit, "\n", sep = "")
  cat("  Baseline operation: ", x$baseline_operation, "\n", sep = "")
  cat("  Sampling interval CV: ", signif(x$timing$cv_dt, 4), "\n", sep = "")
  cat("  Readiness status: ", x$audit$status, "\n", sep = "")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_readiness <- function(x, ...) {
  cat("<gp3bayes_pupil_readiness>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  counts <- table(factor(x$summary$status, levels = c("pass", "review", "fail")))
  cat("  Metrics: ", counts[["pass"]], " pass, ", counts[["review"]],
      " review, ", counts[["fail"]], " fail\n", sep = "")
  cat("  Automatic exclusions: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_readiness <- function(x, ...) pupil_readiness_table(x)

#' @export
as.data.frame.gp3bayes_pupil_measurement_audit <- function(x, ...) pupil_measurement_audit_table(x)

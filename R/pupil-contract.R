# Bayesian dynamic pupillometry foundation ---------------------------------

.gp3p_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3p_null_or <- function(x, y) {
  if (is.null(x)) y else x
}

.gp3p_scalar_character <- function(x, name, optional = FALSE, allow_na = FALSE) {
  if (optional && is.null(x)) return(NULL)
  if (allow_na && length(x) == 1L && is.character(x) && is.na(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .gp3p_stop("`", name, "` must be one non-empty character value",
               if (optional) " or NULL" else "", ".")
  }
  x
}

.gp3p_character_vector <- function(x, name) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    .gp3p_stop("`", name, "` must contain unique non-empty character values.")
  }
  x
}

.gp3p_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3p_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3p_positive <- function(x, name, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    .gp3p_stop("`", name, "` must be one finite positive number.")
  }
  if (integer && x != as.integer(x)) {
    .gp3p_stop("`", name, "` must be a positive integer.")
  }
  if (integer) as.integer(x) else as.numeric(x)
}

.gp3p_probability <- function(x, name, open = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      if (open) (x <= 0 || x >= 1) else (x < 0 || x > 1)) {
    .gp3p_stop("`", name, "` must be a ", if (open) "strict " else "",
               "probability.")
  }
  as.numeric(x)
}

.gp3p_check_columns <- function(data, columns, label = "declared") {
  columns <- unique(columns[!is.na(columns) & nzchar(columns)])
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    .gp3p_stop("Missing ", label, " column(s): ", paste(missing, collapse = ", "), ".")
  }
  invisible(columns)
}

.gp3p_unit_choices <- function() {
  c("millimetres", "metres", "pixels", "arbitrary_units",
    "standardized", "ratio", "proportion_change", "percent_change")
}

.gp3p_time_unit_choices <- function() {
  c("seconds", "milliseconds")
}

.gp3p_match_time_unit <- function(x) {
  x <- .gp3p_scalar_character(x, "time_unit")
  choices <- .gp3p_time_unit_choices()
  if (!x %in% choices) {
    .gp3p_stop("Unsupported `time_unit`: ", x, ". Supported values are: ",
               paste(choices, collapse = ", "), ".")
  }
  x
}

.gp3p_match_unit <- function(x) {
  x <- .gp3p_scalar_character(x, "pupil_unit")
  choices <- .gp3p_unit_choices()
  if (!x %in% choices) {
    .gp3p_stop("Unsupported `pupil_unit`: ", x, ". Supported values are: ",
               paste(choices, collapse = ", "), ".")
  }
  x
}

.gp3p_normalize_unknown <- function(x, name) {
  if (is.null(x)) return(NA_character_)
  if (!is.character(x) || length(x) != 1L || (!is.na(x) && !nzchar(x))) {
    .gp3p_stop("`", name, "` must be one character value, NA, or NULL.")
  }
  if (is.null(x) || is.na(x)) NA_character_ else x
}

.gp3p_contract_required_columns <- function(contract) {
  unname(unlist(contract$mappings[c(
    "outcome", "participant", "trial", "time", "item", "condition",
    "timestamp", "validity", "interpolated", "blink", "gaze_x", "gaze_y",
    "luminance", "contrast"
  )], use.names = FALSE))
}

.gp3p_validate_contract <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_contract")) {
    .gp3p_stop("`contract` must be created by `create_pupil_contract()`.")
  }
  required <- c("contract_version", "family", "model_family", "mappings",
                "pupil_unit", "sampling_frequency", "eye",
                "measurement", "preprocessing", "interpretation_boundaries")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    .gp3p_stop("Malformed pupil contract; missing: ", paste(missing, collapse = ", "), ".")
  }
  invisible(x)
}

#' Create a governed pupil-timecourse contract
#'
#' Records the measurement and analysis declarations required for the
#' restricted Gaussian hierarchical pupil-timecourse family. Contract creation
#' performs no preprocessing, exclusion, correction, model fitting, or
#' psychological interpretation.
#'
#' @param outcome_col Numeric pupil-response column to model.
#' @param participant_col Participant identifier column.
#' @param trial_col Trial identifier column.
#' @param time_col Event-relative time column.
#' @param pupil_unit One of `"millimetres"`, `"metres"`, `"pixels"`,
#'   `"arbitrary_units"`, `"standardized"`, `"ratio"`,
#'   `"proportion_change"`, or `"percent_change"`.
#' @param sampling_frequency Declared nominal sampling frequency in Hz.
#' @param time_unit Unit of `time_col`: `"seconds"` or `"milliseconds"`.
#'   Preparation converts the canonical event-time column to seconds and records
#'   that deterministic conversion.
#' @param item_col Optional item/stimulus identifier.
#' @param condition_col Optional experimental condition.
#' @param timestamp_col Optional absolute or recording timestamp.
#' @param eye Declared channel: `"left"`, `"right"`, `"combined"`, or
#'   `"unknown"`. The function never chooses an eye automatically.
#' @param validity_col,interpolation_col,blink_col Optional measurement-quality
#'   indicator columns.
#' @param left_pupil_col,right_pupil_col Optional paired pupil channels retained
#'   only for left/right disagreement auditing. Either may equal `outcome_col`.
#' @param channel_audit_unit Unit for paired audit channels; defaults to
#'   `pupil_unit` when either paired channel is declared.
#' @param gaze_x_col,gaze_y_col Optional gaze-position columns.
#' @param luminance_col,contrast_col Optional visual-stimulus nuisance columns.
#' @param screen_width,screen_height Optional screen dimensions in declared
#'   screen units; use `NA_real_` when unknown.
#' @param baseline_window Optional two-element event-relative baseline window
#'   expressed in `time_unit`.
#' @param baseline_method Declared upstream/current baseline state: `"none"`,
#'   `"subtract"`, `"divide"`, `"proportion_change"`, `"percent_change"`,
#'   or `"unknown"`.
#' @param baseline_applied Whether baseline correction has already been applied.
#' @param pfe_corrected Whether pupil-foreshortening correction was applied
#'   upstream.
#' @param pfe_method Optional description of the upstream PFE method.
#' @param source_vendor,device_model Optional source metadata. Missing metadata
#'   remain explicitly unknown.
#' @param preprocessing_provenance Optional free-text provenance.
#' @param upstream_package,upstream_version Optional upstream package metadata.
#' @param notes Optional user notes.
#'
#' @return A `gp3bayes_pupil_contract`.
#' @section Governance boundary:
#' The contract records decisions but does not detect blinks, interpolate,
#' smooth, correct PFE, correct luminance, choose a baseline, or exclude data.
#' gp3bayes does not automatically correct blink/data-loss, PFE, gaze-position,
#' luminance, contrast, or baseline decisions recorded upstream.
#' gp3bayes does not infer cognitive load, attention, arousal, stress, emotion,
#' surprise, or effort from a pupil measurement or posterior pupil contrast.
#' Interpretation remains the researcher's responsibility and must be justified
#' by the study design, measurement context, and substantive scientific argument.
#' @examples
#' contract <- create_pupil_contract(
#'   outcome_col = "pupil_mm",
#'   participant_col = "participant_id",
#'   trial_col = "trial_id",
#'   time_col = "event_time",
#'   pupil_unit = "millimetres",
#'   sampling_frequency = 60,
#'   condition_col = "condition",
#'   eye = "combined"
#' )
#' contract
#' @export
create_pupil_contract <- function(
    outcome_col,
    participant_col,
    trial_col,
    time_col,
    pupil_unit,
    sampling_frequency,
    time_unit = c("seconds", "milliseconds"),
    item_col = NULL,
    condition_col = NULL,
    timestamp_col = NULL,
    eye = c("unknown", "left", "right", "combined"),
    left_pupil_col = NULL,
    right_pupil_col = NULL,
    channel_audit_unit = NULL,
    validity_col = NULL,
    interpolation_col = NULL,
    blink_col = NULL,
    gaze_x_col = NULL,
    gaze_y_col = NULL,
    luminance_col = NULL,
    contrast_col = NULL,
    screen_width = NA_real_,
    screen_height = NA_real_,
    baseline_window = NULL,
    baseline_method = c("unknown", "none", "subtract", "divide",
                        "proportion_change", "percent_change"),
    baseline_applied = FALSE,
    pfe_corrected = FALSE,
    pfe_method = NULL,
    source_vendor = NA_character_,
    device_model = NA_character_,
    preprocessing_provenance = NA_character_,
    upstream_package = NA_character_,
    upstream_version = NA_character_,
    notes = character()) {

  outcome_col <- .gp3p_scalar_character(outcome_col, "outcome_col")
  participant_col <- .gp3p_scalar_character(participant_col, "participant_col")
  trial_col <- .gp3p_scalar_character(trial_col, "trial_col")
  time_col <- .gp3p_scalar_character(time_col, "time_col")
  pupil_unit <- .gp3p_match_unit(pupil_unit)
  sampling_frequency <- .gp3p_positive(sampling_frequency, "sampling_frequency")
  time_unit <- match.arg(time_unit)
  eye <- match.arg(eye)
  left_pupil_col <- .gp3p_scalar_character(
    left_pupil_col, "left_pupil_col", optional = TRUE
  )
  right_pupil_col <- .gp3p_scalar_character(
    right_pupil_col, "right_pupil_col", optional = TRUE
  )
  if (is.null(channel_audit_unit)) {
    channel_audit_unit <- if (!is.null(left_pupil_col) || !is.null(right_pupil_col)) {
      pupil_unit
    } else {
      NA_character_
    }
  } else {
    channel_audit_unit <- .gp3p_match_unit(channel_audit_unit)
  }
  baseline_method <- match.arg(baseline_method)
  baseline_applied <- .gp3p_flag(baseline_applied, "baseline_applied")
  pfe_corrected <- .gp3p_flag(pfe_corrected, "pfe_corrected")

  optional_columns <- list(
    item = item_col, condition = condition_col, timestamp = timestamp_col,
    validity = validity_col, interpolated = interpolation_col, blink = blink_col,
    gaze_x = gaze_x_col, gaze_y = gaze_y_col, luminance = luminance_col,
    contrast = contrast_col
  )
  optional_columns <- stats::setNames(
    lapply(names(optional_columns), function(nm) {
      .gp3p_scalar_character(optional_columns[[nm]], paste0(nm, "_col"), optional = TRUE)
    }),
    names(optional_columns)
  )

  mappings <- c(list(
    outcome = outcome_col,
    participant = participant_col,
    trial = trial_col,
    time = time_col
  ), optional_columns)

  declared <- unlist(mappings, use.names = FALSE)
  if (anyDuplicated(declared)) {
    dup <- unique(declared[duplicated(declared)])
    .gp3p_stop("Pupil column mappings must be unique. Duplicated: ",
               paste(dup, collapse = ", "), ".")
  }

  if (!is.null(baseline_window)) {
    if (!is.numeric(baseline_window) || length(baseline_window) != 2L ||
        anyNA(baseline_window) || any(!is.finite(baseline_window)) ||
        baseline_window[1] >= baseline_window[2]) {
      .gp3p_stop("`baseline_window` must be two finite increasing numbers or NULL.")
    }
    baseline_window <- as.numeric(baseline_window)
  }

  dimension_value <- function(x, name) {
    if (length(x) != 1L || !is.numeric(x) || (!is.na(x) && (!is.finite(x) || x <= 0))) {
      .gp3p_stop("`", name, "` must be NA or one finite positive number.")
    }
    as.numeric(x)
  }
  screen_width <- dimension_value(screen_width, "screen_width")
  screen_height <- dimension_value(screen_height, "screen_height")

  notes <- .gp3p_character_vector(notes, "notes")
  pfe_method <- if (is.null(pfe_method)) NA_character_ else
    .gp3p_scalar_character(pfe_method, "pfe_method")

  structure(
    list(
      contract_version = "0.4-pupil-1",
      family = "pupil",
      model_family = "Restricted Gaussian hierarchical pupil time-course",
      likelihood = "Gaussian",
      link = "identity",
      mappings = mappings,
      pupil_unit = pupil_unit,
      sampling_frequency = sampling_frequency,
      time_unit = time_unit,
      eye = eye,
      measurement = list(
        screen_width = screen_width,
        screen_height = screen_height,
        gaze_available = !is.null(mappings$gaze_x) && !is.null(mappings$gaze_y),
        luminance_available = !is.null(mappings$luminance),
        contrast_available = !is.null(mappings$contrast),
        left_pupil_col = left_pupil_col,
        right_pupil_col = right_pupil_col,
        channel_audit_unit = channel_audit_unit
      ),
      preprocessing = list(
        baseline_window = baseline_window,
        baseline_method = baseline_method,
        baseline_applied = baseline_applied,
        pfe_corrected = pfe_corrected,
        pfe_method = pfe_method,
        provenance = .gp3p_normalize_unknown(preprocessing_provenance, "preprocessing_provenance"),
        upstream_package = .gp3p_normalize_unknown(upstream_package, "upstream_package"),
        upstream_version = .gp3p_normalize_unknown(upstream_version, "upstream_version")
      ),
      source = list(
        vendor = .gp3p_normalize_unknown(source_vendor, "source_vendor"),
        device_model = .gp3p_normalize_unknown(device_model, "device_model")
      ),
      notes = notes,
      assumptions = c(
        "The declared pupil channel has a meaningful continuous scale",
        "Event-relative time alignment is scientifically appropriate",
        "The declared hierarchy represents the repeated-measures design",
        "Important temporal dependence is represented or explicitly reviewed",
        "Visual and gaze-related measurement context has been considered"
      ),
      unsupported_uses = c(
        "Automatic blink detection or interpolation",
        "Automatic PFE or luminance correction",
        "Automatic baseline or analysis-window selection",
        "Automatic psychological-state inference",
        "Unrestricted formulas or likelihood families"
      ),
      interpretation_boundaries = c(
        "Pupil response is not itself a named psychological construct",
        "Associations are not causal effects without an identifying design",
        "Passing diagnostics does not establish substantive adequacy",
        "Measurement audits are evidence and never automatic exclusions"
      ),
      fit_performed = FALSE
    ),
    class = c("gp3bayes_pupil_contract", "gp3bayes_contract")
  )
}

#' @export
print.gp3bayes_pupil_contract <- function(x, ...) {
  .gp3p_validate_contract(x)
  cat("<gp3bayes_pupil_contract>\n")
  cat("  Family: pupil\n")
  cat("  Model family: ", x$model_family, "\n", sep = "")
  cat("  Outcome: ", x$mappings$outcome, " [", x$pupil_unit, "]\n", sep = "")
  cat("  Participant: ", x$mappings$participant, "\n", sep = "")
  cat("  Trial: ", x$mappings$trial, "\n", sep = "")
  cat("  Event time: ", x$mappings$time, "\n", sep = "")
  cat("  Nominal sampling: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("  Event-time unit: ", x$time_unit, "\n", sep = "")
  cat("  Eye/channel: ", x$eye, "\n", sep = "")
  cat("  Baseline state: ", x$preprocessing$baseline_method,
      " (applied=", x$preprocessing$baseline_applied, ")\n", sep = "")
  cat("  PFE corrected upstream: ", x$preprocessing$pfe_corrected, "\n", sep = "")
  cat("  Fitting performed: FALSE\n")
  invisible(x)
}

#' Convert a pupil contract to a transparent table
#' @param x A `gp3bayes_pupil_contract`.
#' @param ... Ignored.
#' @return A data frame of contract fields.
#' @export
as.data.frame.gp3bayes_pupil_contract <- function(x, ...) {
  .gp3p_validate_contract(x)
  data.frame(
    field = c(
      "family", "model_family", "outcome", "participant", "trial", "time",
      "item", "condition", "pupil_unit", "sampling_frequency_hz", "time_unit", "eye",
      "baseline_method", "baseline_applied", "pfe_corrected",
      "left_pupil_col", "right_pupil_col", "channel_audit_unit",
      "source_vendor", "device_model", "preprocessing_provenance"
    ),
    value = c(
      x$family, x$model_family, x$mappings$outcome, x$mappings$participant,
      x$mappings$trial, x$mappings$time, .gp3p_null_or(x$mappings$item, NA_character_),
      .gp3p_null_or(x$mappings$condition, NA_character_), x$pupil_unit,
      as.character(x$sampling_frequency), x$time_unit, x$eye,
      x$preprocessing$baseline_method, as.character(x$preprocessing$baseline_applied),
      as.character(x$preprocessing$pfe_corrected),
      .gp3p_null_or(x$measurement$left_pupil_col, NA_character_),
      .gp3p_null_or(x$measurement$right_pupil_col, NA_character_),
      x$measurement$channel_audit_unit,
      x$source$vendor,
      x$source$device_model, x$preprocessing$provenance
    ),
    stringsAsFactors = FALSE
  )
}

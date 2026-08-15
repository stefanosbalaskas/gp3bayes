
.gp3p_scenario_rows <- function(axis, values) {
  if (!length(values)) return(data.frame())
  data.frame(
    axis = axis,
    value = vapply(values, function(x) paste(x, collapse = ","), character(1)),
    stringsAsFactors = FALSE
  )
}

.gp3p_window_to_source_unit <- function(prepared, window) {
  if (is.null(window)) return(NULL)
  if (identical(prepared$source_time_unit, "milliseconds")) {
    as.numeric(window) * 1000
  } else {
    as.numeric(window)
  }
}

.gp3p_rebuild_sensitivity_spec <- function(
    prepared, base_spec, smooth_k, ar, covariates) {
  specify_pupil_timecourse_model(
    prepared,
    temporal_structure = base_spec$temporal_structure,
    smooth_basis_dimension = smooth_k,
    condition_trajectory = base_spec$condition_trajectory,
    autocorrelation = ar,
    participant_trajectory = base_spec$participant_trajectory,
    item_effects = base_spec$item_effects,
    covariates = covariates,
    prior_scales = if (prepared$model_unit %in% c("pixels", "arbitrary_units")) {
      base_spec$prior_scales
    } else NULL
  )
}

#' Create a governed pupil sensitivity suite
#'
#' Declares scientifically consequential alternatives without choosing the
#' alternative that produces the largest effect. The suite is inert until a
#' scenario is explicitly materialized or results are supplied for comparison.
#'
#' @param specification Baseline pupil model specification.
#' @param baseline_windows List of alternative two-element baseline windows.
#' @param baseline_window_operation Optional baseline transformation to pair
#'   with `baseline_windows` when the baseline specification used `"none"`.
#'   This must be declared explicitly; gp3bayes never chooses one.
#' @param baseline_operations Alternative baseline transformations.
#' @param interpolation_policy `"retain"` and/or `"exclude_flagged"`.
#' @param blink_adjacent_margins Non-negative margins in seconds; zero means no
#'   blink-adjacent deletion.
#' @param gaze_adjustment `"none"` and/or `"declared_covariates"`.
#' @param luminance_adjustment `"none"` and/or `"declared_covariate"`.
#' @param pfe_prepared Optional named list of explicitly prepared alternative
#'   pupil series (for example upstream corrected and uncorrected versions).
#'   gp3bayes does not perform PFE correction.
#' @param smooth_basis_dimensions Alternative approved basis dimensions.
#' @param autocorrelation Alternative `"none"`/`"ar1"` structures.
#' @param analysis_windows List of declared estimand windows in canonical
#'   event-time seconds.
#' @return A `gp3bayes_pupil_sensitivity` plan.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
create_pupil_sensitivity_suite <- function(
    specification,
    baseline_windows = list(),
    baseline_window_operation = NULL,
    baseline_operations = character(),
    interpolation_policy = character(),
    blink_adjacent_margins = numeric(),
    gaze_adjustment = character(),
    luminance_adjustment = character(),
    pfe_prepared = list(),
    smooth_basis_dimensions = integer(),
    autocorrelation = character(),
    analysis_windows = list()) {

  if (!inherits(specification, "gp3bayes_pupil_model_specification")) {
    .gp3p_stop("`specification` must be a pupil model specification.")
  }
  approved_baseline <- c(
    "none", "subtract", "divide", "proportion_change", "percent_change"
  )
  if (length(baseline_operations) &&
      any(!baseline_operations %in% approved_baseline)) {
    .gp3p_stop("Unsupported baseline operation in sensitivity suite.")
  }
  if (!is.null(baseline_window_operation)) {
    baseline_window_operation <- .gp3p_scalar_character(
      baseline_window_operation, "baseline_window_operation"
    )
    if (!baseline_window_operation %in% setdiff(approved_baseline, "none")) {
      .gp3p_stop(
        "`baseline_window_operation` must be an explicit non-`none` baseline transformation."
      )
    }
  }
  if (length(baseline_windows) &&
      identical(specification$prepared$baseline_operation, "none") &&
      is.null(baseline_window_operation)) {
    .gp3p_stop(
      "Baseline-window sensitivity requires `baseline_window_operation` when ",
      "the baseline specification used `baseline_operation = \"none\"`."
    )
  }
  if (length(interpolation_policy) &&
      any(!interpolation_policy %in% c("retain", "exclude_flagged"))) {
    .gp3p_stop("Unsupported interpolation policy.")
  }
  if (length(gaze_adjustment) &&
      any(!gaze_adjustment %in% c("none", "declared_covariates"))) {
    .gp3p_stop("Unsupported gaze adjustment.")
  }
  if (length(luminance_adjustment) &&
      any(!luminance_adjustment %in% c("none", "declared_covariate"))) {
    .gp3p_stop("Unsupported luminance adjustment.")
  }
  if (length(autocorrelation) && any(!autocorrelation %in% c("none", "ar1"))) {
    .gp3p_stop("Unsupported autocorrelation sensitivity value.")
  }
  if (length(smooth_basis_dimensions) &&
      any(!is.finite(smooth_basis_dimensions) | smooth_basis_dimensions < 3 |
          smooth_basis_dimensions != as.integer(smooth_basis_dimensions))) {
    .gp3p_stop("Sensitivity basis dimensions must be integers of at least 3.")
  }
  if (length(blink_adjacent_margins) &&
      any(!is.finite(blink_adjacent_margins) | blink_adjacent_margins < 0)) {
    .gp3p_stop("Blink-adjacent margins must be finite and non-negative.")
  }
  for (w in c(baseline_windows, analysis_windows)) .gp3p_validate_window(w)

  if (length(pfe_prepared)) {
    if (is.null(names(pfe_prepared)) || any(!nzchar(names(pfe_prepared))) ||
        anyDuplicated(names(pfe_prepared))) {
      .gp3p_stop("`pfe_prepared` must be a uniquely named list.")
    }
    if (!all(vapply(
      pfe_prepared, inherits, logical(1), "gp3bayes_pupil_prepared"
    ))) {
      .gp3p_stop("Every `pfe_prepared` alternative must be a prepared pupil object.")
    }
    base_unit <- specification$prepared$model_unit
    bad_units <- vapply(
      pfe_prepared, function(z) !identical(z$model_unit, base_unit), logical(1)
    )
    if (any(bad_units)) {
      .gp3p_stop("All PFE sensitivity alternatives must use the baseline model unit.")
    }
  }

  pieces <- list(
    .gp3p_scenario_rows("baseline_window", baseline_windows),
    .gp3p_scenario_rows("baseline_operation", as.list(baseline_operations)),
    .gp3p_scenario_rows("interpolation_policy", as.list(interpolation_policy)),
    .gp3p_scenario_rows("blink_adjacent_margin", as.list(blink_adjacent_margins)),
    .gp3p_scenario_rows("gaze_adjustment", as.list(gaze_adjustment)),
    .gp3p_scenario_rows("luminance_adjustment", as.list(luminance_adjustment)),
    .gp3p_scenario_rows("pfe_prepared", as.list(names(pfe_prepared))),
    .gp3p_scenario_rows(
      "smooth_basis_dimension", as.list(smooth_basis_dimensions)
    ),
    .gp3p_scenario_rows("autocorrelation", as.list(autocorrelation)),
    .gp3p_scenario_rows("analysis_window", analysis_windows)
  )
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0L]
  scenarios <- if (length(pieces)) do.call(rbind, pieces) else {
    data.frame(axis = character(), value = character(), stringsAsFactors = FALSE)
  }
  scenarios$scenario_id <- if (nrow(scenarios)) {
    sprintf("S%03d", seq_len(nrow(scenarios)))
  } else character()
  scenarios <- scenarios[, c("scenario_id", "axis", "value"), drop = FALSE]

  structure(
    list(
      suite_version = "0.4-pupil-1",
      family = "pupil",
      baseline_specification = specification,
      baseline_window_operation = .gp3p_null_or(
        baseline_window_operation,
        if (!identical(specification$prepared$baseline_operation, "none")) {
          specification$prepared$baseline_operation
        } else NULL
      ),
      pfe_prepared = pfe_prepared,
      scenarios = scenarios,
      selection_rule = "none",
      automatic_effect_maximization = FALSE,
      pfe_correction_performed = FALSE,
      results = list()
    ),
    class = c("gp3bayes_pupil_sensitivity", "gp3bayes_sensitivity_suite")
  )
}

#' Materialize one declared pupil sensitivity scenario
#'
#' Creates the alternate prepared/specification state for a declared scenario.
#' This does not fit the model. Analysis-window scenarios are returned as
#' estimand instructions. PFE scenarios select only a user-supplied upstream
#' prepared alternative; no PFE correction is performed by gp3bayes.
#'
#' @param suite Pupil sensitivity suite.
#' @param scenario_id Scenario identifier from `pupil_sensitivity_table()`.
#' @return A list containing the materialized specification and/or declared
#'   estimand window.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
materialize_pupil_sensitivity_scenario <- function(suite, scenario_id) {
  if (!inherits(suite, "gp3bayes_pupil_sensitivity")) {
    .gp3p_stop("`suite` must be a pupil sensitivity suite.")
  }
  scenario_id <- .gp3p_scalar_character(scenario_id, "scenario_id")
  row <- suite$scenarios[
    suite$scenarios$scenario_id == scenario_id, , drop = FALSE
  ]
  if (nrow(row) != 1L) .gp3p_stop("Unknown or duplicated `scenario_id`.")
  base_spec <- suite$baseline_specification
  prepared <- base_spec$prepared
  d <- prepared$data
  axis <- row$axis
  value <- row$value
  estimand_window <- NULL

  baseline_operation <- prepared$baseline_operation
  baseline_window <- prepared$baseline_window
  keep <- rep(TRUE, nrow(d))
  covariates <- base_spec$covariates
  smooth_k <- base_spec$smooth_basis_dimension
  ar <- base_spec$autocorrelation
  alternate_prepared <- NULL

  if (identical(axis, "baseline_window")) {
    baseline_window <- as.numeric(strsplit(value, ",", fixed = TRUE)[[1L]])
    baseline_operation <- suite$baseline_window_operation
  } else if (identical(axis, "baseline_operation")) {
    baseline_operation <- value
  } else if (identical(axis, "interpolation_policy") &&
             identical(value, "exclude_flagged")) {
    if (!".interpolated" %in% names(d) || all(is.na(d$.interpolated))) {
      .gp3p_stop(
        "Interpolation sensitivity requested but no interpolation indicator is available."
      )
    }
    keep <- is.na(d$.interpolated) | !d$.interpolated
  } else if (identical(axis, "blink_adjacent_margin")) {
    margin <- as.numeric(value)
    if (!".blink" %in% names(d) || all(is.na(d$.blink))) {
      .gp3p_stop(
        "Blink-adjacent sensitivity requested but no blink indicator is available."
      )
    }
    if (margin > 0) {
      for (series in levels(d$.series_id)) {
        idx <- which(d$.series_id == series)
        bt <- d$.event_time[idx][which(d$.blink[idx] %in% TRUE)]
        if (length(bt)) {
          exclude <- vapply(
            d$.event_time[idx],
            function(t) any(abs(t - bt) <= margin),
            logical(1)
          )
          keep[idx[exclude]] <- FALSE
        }
      }
    }
  } else if (identical(axis, "gaze_adjustment") &&
             identical(value, "declared_covariates")) {
    if (!all(c(".gaze_x", ".gaze_y") %in% names(d))) {
      .gp3p_stop("Gaze sensitivity requested but gaze coordinates are unavailable.")
    }
    covariates <- unique(c(covariates, ".gaze_x", ".gaze_y"))
  } else if (identical(axis, "luminance_adjustment") &&
             identical(value, "declared_covariate")) {
    if (!".luminance" %in% names(d)) {
      .gp3p_stop("Luminance sensitivity requested but luminance is unavailable.")
    }
    covariates <- unique(c(covariates, ".luminance"))
  } else if (identical(axis, "pfe_prepared")) {
    alternate_prepared <- suite$pfe_prepared[[value]]
    if (is.null(alternate_prepared)) {
      .gp3p_stop("Declared PFE prepared alternative is unavailable.")
    }
  } else if (identical(axis, "smooth_basis_dimension")) {
    smooth_k <- as.integer(value)
  } else if (identical(axis, "autocorrelation")) {
    ar <- value
  } else if (identical(axis, "analysis_window")) {
    estimand_window <- as.numeric(strsplit(value, ",", fixed = TRUE)[[1L]])
  }

  if (identical(axis, "pfe_prepared")) {
    new_prepared <- alternate_prepared
    new_spec <- .gp3p_rebuild_sensitivity_spec(
      new_prepared, base_spec, smooth_k, ar, covariates
    )
  } else if (!identical(axis, "analysis_window")) {
    source_data <- d[keep, , drop = FALSE]
    source_window <- .gp3p_window_to_source_unit(prepared, baseline_window)
    new_prepared <- prepare_pupil_timecourse(
      source_data,
      prepared$contract,
      baseline_operation = baseline_operation,
      baseline_window = source_window,
      output_unit = if (
        !identical(prepared$source_unit, prepared$model_unit) &&
        prepared$model_unit %in% c("millimetres", "metres")
      ) prepared$model_unit else NULL,
      max_rows = max(2000000L, nrow(source_data))
    )
    new_spec <- .gp3p_rebuild_sensitivity_spec(
      new_prepared, base_spec, smooth_k, ar, covariates
    )
  } else {
    new_prepared <- prepared
    new_spec <- base_spec
  }

  list(
    scenario_id = scenario_id,
    axis = axis,
    value = value,
    prepared = new_prepared,
    specification = new_spec,
    analysis_window = estimand_window,
    fit_performed = FALSE,
    pfe_correction_performed = FALSE
  )
}

#' Compare declared pupil sensitivity estimands
#'
#' Combines already-computed pupil estimands by named scenario. No scenario is
#' ranked or selected.
#'
#' @param results Named list of `gp3bayes_pupil_estimand` objects.
#' @return A `gp3bayes_pupil_sensitivity_comparison`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
compare_pupil_sensitivity_estimands <- function(results) {
  if (!is.list(results) || !length(results) || is.null(names(results)) ||
      any(!nzchar(names(results)))) {
    .gp3p_stop("`results` must be a non-empty named list of pupil estimands.")
  }
  if (!all(vapply(
    results, inherits, logical(1), "gp3bayes_pupil_estimand"
  ))) {
    .gp3p_stop("Every sensitivity result must be a pupil estimand.")
  }
  rows <- lapply(
    names(results),
    function(nm) {
      tab <- as.data.frame(results[[nm]])
      tab$scenario_id <- nm
      tab
    }
  )
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  structure(
    list(table = table, automatic_selection = FALSE),
    class = "gp3bayes_pupil_sensitivity_comparison"
  )
}

#' Extract pupil sensitivity scenarios or comparison results
#' @param x A pupil sensitivity suite or sensitivity comparison.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_sensitivity_table <- function(x) {
  if (inherits(x, "gp3bayes_pupil_sensitivity")) return(x$scenarios)
  if (inherits(x, "gp3bayes_pupil_sensitivity_comparison")) return(x$table)
  .gp3p_stop("`x` must be a pupil sensitivity suite or comparison.")
}

#' @export
print.gp3bayes_pupil_sensitivity <- function(x, ...) {
  cat("<gp3bayes_pupil_sensitivity>\n")
  cat("  Declared scenarios: ", nrow(x$scenarios), "\n", sep = "")
  cat("  PFE correction performed by gp3bayes: FALSE\n")
  cat("  Automatic selection: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_sensitivity <- function(x, ...) {
  pupil_sensitivity_table(x)
}

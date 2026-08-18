# Advanced governed pupil model specifications for gp3bayes 0.5.

#' Create a governed measurement-uncertainty specification
#'
#' Declares known standard-error columns for pupil covariates and/or the pupil
#' response. The object records uncertainty; it does not alter or impute data.
#'
#' @param baseline_error,luminance_error,gaze_error Optional standard-error
#'   column names for common adjustment variables.
#' @param response_error Optional known standard-error column for the pupil
#'   response.
#' @param covariate_errors Optional named character vector mapping arbitrary
#'   covariate names to standard-error columns.
#' @return A `gp3bayes_pupil_measurement_model` object.
#' @export
create_pupil_measurement_model <- function(
    baseline_error = NULL,
    luminance_error = NULL,
    gaze_error = NULL,
    response_error = NULL,
    covariate_errors = NULL) {

  scalar_chr_or_null <- function(x, nm) {
    if (is.null(x)) return(NULL)
    if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
      stop("`", nm, "` must be NULL or one non-empty column name.", call. = FALSE)
    }
    x
  }

  baseline_error <- scalar_chr_or_null(baseline_error, "baseline_error")
  luminance_error <- scalar_chr_or_null(luminance_error, "luminance_error")
  gaze_error <- scalar_chr_or_null(gaze_error, "gaze_error")
  response_error <- scalar_chr_or_null(response_error, "response_error")

  if (is.null(covariate_errors)) covariate_errors <- character()
  if (!is.character(covariate_errors) || (length(covariate_errors) && is.null(names(covariate_errors)))) {
    stop("`covariate_errors` must be NULL or a named character vector: covariate = standard_error_column.", call. = FALSE)
  }
  if (length(covariate_errors) && any(!nzchar(names(covariate_errors)) | !nzchar(covariate_errors))) {
    stop("All `covariate_errors` names and values must be non-empty.", call. = FALSE)
  }

  common <- character()

  if (!is.null(baseline_error)) {
    common["baseline"] <- baseline_error
  }

  if (!is.null(luminance_error)) {
    common["luminance"] <- luminance_error
  }

  if (!is.null(gaze_error)) {
    common["gaze_eccentricity"] <- gaze_error
  }

  all_errors <- c(
    common,
    covariate_errors
  )
  if (anyDuplicated(names(all_errors))) {
    stop("Duplicate measurement-error declarations for the same covariate.", call. = FALSE)
  }

  structure(
    list(
      covariate_errors = all_errors,
      response_error = response_error,
      interpretation = paste(
        "Known uncertainty is propagated conditionally on the declared measurement model.",
        "The declaration does not validate calibration, measurement unbiasedness, or causal interpretation."
      )
    ),
    class = "gp3bayes_pupil_measurement_model"
  )
}

#' Create a governed missingness specification
#'
#' @param response Either `"exclude"` or `"model"`. `"model"` uses brms
#'   missing-response syntax and is interpreted under an explicitly declared
#'   MAR-oriented modelling assumption, not as proof that MAR holds.
#' @param predictors Character vector of predictor columns whose missing values
#'   should be modelled jointly.
#' @param assumptions Currently only `"MAR"` is accepted for modelling.
#' @param auxiliary_predictors Optional fully observed columns to use in the
#'   missing-predictor submodels.
#' @return A `gp3bayes_pupil_missingness_spec` object.
#' @export
create_pupil_missingness_spec <- function(
    response = c("exclude", "model"),
    predictors = character(),
    assumptions = "MAR",
    auxiliary_predictors = character()) {

  response <- match.arg(response)
  if (!is.character(predictors)) stop("`predictors` must be character.", call. = FALSE)
  if (!is.character(auxiliary_predictors)) stop("`auxiliary_predictors` must be character.", call. = FALSE)
  predictors <- unique(predictors[nzchar(predictors)])
  auxiliary_predictors <- unique(auxiliary_predictors[nzchar(auxiliary_predictors)])

  if (!identical(assumptions, "MAR")) {
    stop(
      "gp3bayes 0.5 does not implement MNAR identification. `assumptions` must be \"MAR\"; sensitivity to departures from MAR must be handled separately.",
      call. = FALSE
    )
  }

  structure(
    list(
      response = response,
      predictors = predictors,
      assumptions = assumptions,
      auxiliary_predictors = auxiliary_predictors,
      interpretation = paste(
        "Missingness modelling is assumption-conditional.",
        "Fitting a missing-data model does not establish that MAR is true."
      )
    ),
    class = "gp3bayes_pupil_missingness_spec"
  )
}

#' Create a Gaussian-process configuration for pupil trajectories
#'
#' @param kernel GP covariance kernel.
#' @param basis `"exact"` or `"approximate"`.
#' @param k Number of Hilbert-space basis functions when `basis = "approximate"`.
#' @param scale Whether brms should internally scale GP predictors.
#' @return A `gp3bayes_pupil_gp_spec` object.
#' @export
create_pupil_gp_spec <- function(
    kernel = c("matern32", "matern52", "exp_quad"),
    basis = c("approximate", "exact"),
    k = 30L,
    scale = TRUE) {

  kernel <- match.arg(kernel)
  basis <- match.arg(basis)
  .p05_assert_flag(scale, "scale")

  if (basis == "approximate") {
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 5 || k > 200 || k != as.integer(k)) {
      stop("For approximate GPs, `k` must be an integer from 5 to 200.", call. = FALSE)
    }
    k <- as.integer(k)
  } else {
    k <- NA_integer_
  }

  structure(
    list(kernel = kernel, basis = basis, k = k, scale = scale),
    class = "gp3bayes_pupil_gp_spec"
  )
}

#' Create an explicit bounded ARMA configuration
#'
#' @param p Autoregressive order, constrained to 0--3.
#' @param q Moving-average order, constrained to 0--2.
#' @param covariance Logical; request covariance-form ARMA. In gp3bayes 0.5
#'   this is permitted only for order (1,0), (0,1), or (1,1).
#' @return A `gp3bayes_pupil_arma_spec` object.
#' @export
create_pupil_arma_spec <- function(p = 1L, q = 0L, covariance = FALSE) {
  p <- .p05_assert_integerish(p, "p", 0L, 3L)
  q <- .p05_assert_integerish(q, "q", 0L, 2L)
  .p05_assert_flag(covariance, "covariance")
  if (p == 0L && q == 0L) stop("Use `autocorrelation = \"none\"` instead of ARMA(0,0).", call. = FALSE)
  if (covariance && !(p <= 1L && q <= 1L)) {
    stop("Covariance-form ARMA is restricted to orders no greater than (1,1) in gp3bayes 0.5.", call. = FALSE)
  }
  structure(list(p = p, q = q, covariance = covariance), class = "gp3bayes_pupil_arma_spec")
}

.p05_autocorrelation_spec <- function(x) {
  if (inherits(x, "gp3bayes_pupil_arma_spec")) return(x)
  x <- match.arg(x, c("none", "ar1", "ar2", "arma11"))
  switch(
    x,
    none = NULL,
    ar1 = create_pupil_arma_spec(1L, 0L, FALSE),
    ar2 = create_pupil_arma_spec(2L, 0L, FALSE),
    arma11 = create_pupil_arma_spec(1L, 1L, FALSE)
  )
}

.p05_spec_compatibility <- function(family, autocorrelation, missingness_model, measurement_model) {
  issues <- data.frame(
    severity = character(),
    code = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  add <- function(severity, code, message) {
    issues <<- rbind(issues, data.frame(severity = severity, code = code, message = message, stringsAsFactors = FALSE))
  }

  if (family == "student" && !is.null(autocorrelation)) {
    add(
      "failure", "student_arma_combination",
      paste(
        "gp3bayes 0.5 deliberately does not combine Student-t observation models with residual ARMA in the governed interface.",
        "Fit Student-t robustness and Gaussian ARMA as separate candidate specifications and compare them predictively."
      )
    )
  }
  if (!is.null(missingness_model) && missingness_model$response == "model" && !is.null(autocorrelation)) {
    add(
      "failure", "missing_response_arma",
      "Model-based missing pupil responses cannot currently be combined with residual ARMA because missing time points change the residual-series contract."
    )
  }
  if (!is.null(measurement_model) && !inherits(measurement_model, "gp3bayes_pupil_measurement_model")) {
    add("failure", "measurement_object", "`measurement_model` must come from create_pupil_measurement_model().")
  }
  if (!is.null(missingness_model) && !inherits(missingness_model, "gp3bayes_pupil_missingness_spec")) {
    add("failure", "missingness_object", "`missingness_model` must come from create_pupil_missingness_spec().")
  }
  issues
}

#' Specify an advanced governed pupil time-course model
#'
#' Builds the 0.5 advanced model contract without compiling or fitting Stan.
#' The function is additive to the frozen 0.4 API: it consumes the same prepared
#' pupil data but returns a distinct advanced specification.
#'
#' @param prepared A prepared 0.4 pupil object or compatible data frame.
#' @param temporal_structure `"smooth"`, `"linear"`, or `"gaussian_process"`.
#' @param family `"gaussian"` or robust `"student"`.
#' @param residual_scale Residual-scale model: constant, condition, time, or
#'   condition-by-time.
#' @param distribution Optional object from [specify_pupil_distribution()].
#'   When supplied, its family and residual-scale declarations override the
#'   corresponding scalar arguments.
#' @param smooth_basis_dimension Basis dimension for smooth mean trajectories.
#' @param gp_spec A GP configuration from [create_pupil_gp_spec()].
#' @param condition_trajectory Whether condition-specific trajectories are
#'   included. Defaults to TRUE when a condition column exists.
#' @param autocorrelation One of `"none"`, `"ar1"`, `"ar2"`, `"arma11"`, or
#'   a bounded object from [create_pupil_arma_spec()].
#' @param participant_trajectory `"none"` or `"factor_smooth"`.
#' @param item_effects Include a random item intercept when an item column exists.
#' @param covariates Additional declared covariates.
#' @param measurement_model Optional known-uncertainty declaration.
#' @param missingness_model Optional MAR-oriented missingness declaration.
#' @param prior_scales Optional named numeric prior-scale overrides.
#' @param predictive_target Declared prediction target inherited from the 0.4
#'   validation vocabulary.
#' @param allow_high_complexity Permit specifications flagged by the complexity
#'   audit. This is an explicit opt-in, not automatic model approval.
#' @return A `gp3bayes_pupil_advanced_specification` object.
#' @export
specify_advanced_pupil_timecourse_model <- function(
    prepared,
    temporal_structure = c("smooth", "linear", "gaussian_process"),
    family = c("gaussian", "student"),
    residual_scale = c("constant", "condition", "time", "condition_time"),
    distribution = NULL,
    smooth_basis_dimension = 10L,
    gp_spec = create_pupil_gp_spec(),
    condition_trajectory = NULL,
    autocorrelation = c("none", "ar1", "ar2", "arma11"),
    participant_trajectory = c("none", "factor_smooth"),
    item_effects = NULL,
    covariates = character(),
    measurement_model = NULL,
    missingness_model = NULL,
    prior_scales = NULL,
    predictive_target = c("new_trial_known_participant", "new_participant", "future_segment", "new_sample_known_trial"),
    allow_high_complexity = FALSE) {

  smooth_k_was_missing <- missing(smooth_basis_dimension)

  temporal_structure <- match.arg(temporal_structure)
  if (!is.null(distribution)) {
    if (!inherits(distribution, "gp3bayes_pupil_distribution_spec")) {
      stop("`distribution` must come from specify_pupil_distribution().", call. = FALSE)
    }
    family <- distribution$family
    residual_scale <- distribution$residual_scale
  } else {
    family <- match.arg(family)
    residual_scale <- match.arg(residual_scale)
  }
  participant_trajectory <- match.arg(participant_trajectory)
  predictive_target <- match.arg(predictive_target)
  .p05_assert_flag(allow_high_complexity, "allow_high_complexity")

  data <- .p05_data(prepared)
  mapping <- .p05_mapping(prepared, require_condition = FALSE)

  if (!is.numeric(data[[mapping$response]])) stop("Pupil response must be numeric.", call. = FALSE)
  if (!is.numeric(data[[mapping$time]])) stop("Time must be numeric.", call. = FALSE)
  if (all(is.na(data[[mapping$response]]))) stop("Pupil response is entirely missing.", call. = FALSE)
  if (anyNA(data[[mapping$participant]])) stop("Participant identifiers must be non-missing.", call. = FALSE)
  if (any(!is.finite(data[[mapping$time]]))) stop("Time values must be finite and non-missing.", call. = FALSE)
  if (!is.null(mapping$trial) && anyNA(data[[mapping$trial]])) stop("Trial identifiers must be non-missing when a trial column is present.", call. = FALSE)

  n_condition <- if (is.null(mapping$condition)) 0L else length(unique(data[[mapping$condition]][!is.na(data[[mapping$condition]])]))
  if (is.null(condition_trajectory)) condition_trajectory <- n_condition >= 2L
  .p05_assert_flag(condition_trajectory, "condition_trajectory")
  if (condition_trajectory && is.null(mapping$condition)) {
    stop("A condition-specific trajectory was requested but no condition column was resolved.", call. = FALSE)
  }
  if (condition_trajectory && n_condition < 2L) {
    stop("A condition-specific trajectory requires at least two observed condition levels.", call. = FALSE)
  }
  if (residual_scale %in% c("condition", "condition_time") && n_condition < 2L) {
    stop("Condition-dependent residual scale requires at least two observed condition levels.", call. = FALSE)
  }

  smooth_basis_dimension <- .p05_assert_integerish(smooth_basis_dimension, "smooth_basis_dimension", 4L, 100L)

  requested_smooth_basis_dimension <- smooth_basis_dimension
  smooth_basis_support <- NA_integer_
  smooth_basis_adjusted <- FALSE

  if (temporal_structure == "smooth") {

    time_value <- data[[mapping$time]]
    observed_time <- is.finite(time_value)

    global_support <- length(
      unique(
        time_value[observed_time]
      )
    )

    support_values <- as.integer(global_support)

    if (
      !is.null(mapping$condition) &&
      condition_trajectory
    ) {

      condition_value <- data[[mapping$condition]]
      ok <- observed_time & !is.na(condition_value)

      by_condition_support <- tapply(
        time_value[ok],
        condition_value[ok],
        function(z) {
          length(
            unique(
              z[is.finite(z)]
            )
          )
        }
      )

      by_condition_support <- as.integer(
        by_condition_support[
          is.finite(by_condition_support)
        ]
      )

      if (!length(by_condition_support)) {
        stop(
          paste(
            "Could not determine condition-specific temporal",
            "support for the advanced smooth model."
          ),
          call. = FALSE
        )
      }

      support_values <- c(
        support_values,
        by_condition_support
      )
    }

    if (identical(participant_trajectory, "factor_smooth")) {

      participant_value <- data[[mapping$participant]]
      ok <- observed_time & !is.na(participant_value)

      by_participant_support <- tapply(
        time_value[ok],
        participant_value[ok],
        function(z) {
          length(
            unique(
              z[is.finite(z)]
            )
          )
        }
      )

      by_participant_support <- as.integer(
        by_participant_support[
          is.finite(by_participant_support)
        ]
      )

      if (!length(by_participant_support)) {
        stop(
          paste(
            "Could not determine participant-specific temporal",
            "support for the requested factor smooth."
          ),
          call. = FALSE
        )
      }

      support_values <- c(
        support_values,
        by_participant_support
      )
    }

    time_support <- min(support_values)

    # Reserve one dimension for the identifiability constraint.
    smooth_basis_support <- as.integer(
      time_support - 1L
    )

    if (smooth_basis_support < 4L) {
      stop(
        paste0(
          "The advanced smooth model has insufficient temporal support. ",
          "At least five unique supported time values are required; ",
          "the smallest relevant support is ",
          time_support,
          "."
        ),
        call. = FALSE
      )
    }

    if (smooth_basis_dimension > smooth_basis_support) {

      if (smooth_k_was_missing) {

        smooth_basis_dimension <- smooth_basis_support
        smooth_basis_adjusted <- TRUE

      } else {

        stop(
          paste0(
            "`smooth_basis_dimension` = ",
            smooth_basis_dimension,
            " exceeds the governed temporal support of this design. ",
            "The maximum supported value is ",
            smooth_basis_support,
            " based on a minimum relevant support of ",
            time_support,
            " unique time values. Specify a smaller basis dimension explicitly."
          ),
          call. = FALSE
        )
      }
    }
  }


  if (temporal_structure == "gaussian_process" && !inherits(gp_spec, "gp3bayes_pupil_gp_spec")) {
    stop("`gp_spec` must come from create_pupil_gp_spec().", call. = FALSE)
  }

  ac <- if (inherits(autocorrelation, "gp3bayes_pupil_arma_spec")) {
    autocorrelation
  } else {
    .p05_autocorrelation_spec(match.arg(autocorrelation))
  }

  if (is.null(item_effects)) item_effects <- !is.null(mapping$item)
  .p05_assert_flag(item_effects, "item_effects")
  if (item_effects && is.null(mapping$item)) stop("`item_effects = TRUE` but no item column was resolved.", call. = FALSE)
  if (item_effects && anyNA(data[[mapping$item]])) stop("Item identifiers must be non-missing when item effects are requested.", call. = FALSE)

  protected <- unlist(mapping, use.names = FALSE)
  protected <- protected[!is.na(protected) & nzchar(protected)]
  covariates <- .p05_check_covariates(data, covariates, protected)

  if (!is.null(prior_scales)) {
    if (!is.numeric(prior_scales) || is.null(names(prior_scales)) || any(!is.finite(prior_scales)) || any(prior_scales <= 0)) {
      stop("`prior_scales` must be NULL or a named positive finite numeric vector.", call. = FALSE)
    }
  }

  if (!is.null(measurement_model)) {
    if (!inherits(measurement_model, "gp3bayes_pupil_measurement_model")) {
      stop("`measurement_model` must come from create_pupil_measurement_model().", call. = FALSE)
    }
    declared <- names(measurement_model$covariate_errors)
    resolved_declared <- vapply(
      declared,
      function(z) {
        if (z %in% names(data)) return(z)
        hit <- switch(
          z,
          baseline = c("baseline", "baseline_pupil", "pupil_baseline", "baseline_value"),
          luminance = c("luminance", "screen_luminance", "stimulus_luminance", "luminance_value"),
          gaze_eccentricity = c("gaze_eccentricity", "eccentricity", "gaze_distance", "gaze_ecc"),
          z
        )
        hit <- hit[hit %in% names(data)]
        if (length(hit)) hit[[1L]] else NA_character_
      },
      character(1L)
    )
    if (anyNA(resolved_declared)) {
      stop("Could not resolve measurement-error covariate(s): ", paste(declared[is.na(resolved_declared)], collapse = ", "), ".", call. = FALSE)
    }
    unknown <- setdiff(resolved_declared, covariates)
    if (length(unknown)) {
      stop("Measurement-error covariates must also be declared in `covariates`: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    se_cols <- c(unname(measurement_model$covariate_errors), measurement_model$response_error)
    se_cols <- se_cols[!is.na(se_cols) & nzchar(se_cols)]
    miss_se <- setdiff(se_cols, names(data))
    if (length(miss_se)) stop("Unknown measurement-error standard-error column(s): ", paste(miss_se, collapse = ", "), ".", call. = FALSE)
    bad_se <- vapply(
      se_cols,
      function(z) {
        x <- data[[z]]
        !is.numeric(x) || any(is.na(x) | !is.finite(x) | x <= 0)
      },
      logical(1L)
    )
    if (any(bad_se)) {
      stop(
        "Measurement-error standard-error columns must be numeric, finite, strictly positive, and non-missing: ",
        paste(se_cols[bad_se], collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  if (!is.null(missingness_model)) {
    if (!inherits(missingness_model, "gp3bayes_pupil_missingness_spec")) {
      stop("`missingness_model` must come from create_pupil_missingness_spec().", call. = FALSE)
    }
    needed <- unique(c(missingness_model$predictors, missingness_model$auxiliary_predictors))
    miss <- setdiff(needed, names(data))
    if (length(miss)) stop("Unknown missingness-model column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
    pred_not_cov <- setdiff(missingness_model$predictors, covariates)
    if (length(pred_not_cov)) stop("Missing predictors must also be listed in `covariates`: ", paste(pred_not_cov, collapse = ", "), ".", call. = FALSE)
  }

  compatibility <- .p05_spec_compatibility(family, ac, missingness_model, measurement_model)
  if (any(compatibility$severity == "failure")) {
    stop(paste(compatibility$message[compatibility$severity == "failure"], collapse = "\n"), call. = FALSE)
  }

  spec <- structure(
    list(
      version = "0.5.0.9000",
      prepared = prepared,
      data = data,
      mapping = mapping,
      temporal_structure = temporal_structure,
      family = family,
      residual_scale = residual_scale,
      smooth_basis_dimension = smooth_basis_dimension,
      smooth_basis_dimension_requested = requested_smooth_basis_dimension,
      smooth_basis_dimension_effective = smooth_basis_dimension,
      smooth_basis_support = smooth_basis_support,
      smooth_basis_adjusted = smooth_basis_adjusted,
      gp_spec = if (temporal_structure == "gaussian_process") gp_spec else NULL,
      condition_trajectory = condition_trajectory,
      autocorrelation = ac,
      participant_trajectory = participant_trajectory,
      item_effects = item_effects,
      covariates = covariates,
      measurement_model = measurement_model,
      missingness_model = missingness_model,
      prior_scales = prior_scales,
      predictive_target = predictive_target,
      allow_high_complexity = allow_high_complexity,
      compatibility = compatibility,
      backend = "none",
      fit_performed = FALSE,
      governance = c(
        "No automatic preprocessing, interpolation, exclusion, or model selection.",
        "No automatic cognitive-state, causal, or adequacy interpretation.",
        "Measurement and missingness models remain assumption-conditional.",
        "Predictive comparison is tied to an explicitly declared target."
      )
    ),
    class = "gp3bayes_pupil_advanced_specification"
  )

  budget <- audit_pupil_computational_budget(spec)
  if (identical(budget$overall_status, "high") && !allow_high_complexity) {
    stop(
      paste0(
        "The requested specification exceeds the default gp3bayes 0.5 complexity budget: ",
        paste(budget$checks$message[budget$checks$status == "high"], collapse = "; "),
        ". Review audit_pupil_computational_budget() and set `allow_high_complexity = TRUE` only after an explicit computational/scientific decision."
      ),
      call. = FALSE
    )
  }

  spec$complexity_audit <- budget
  spec
}

#' Audit computational complexity before fitting an advanced pupil model
#'
#' @param x An advanced specification.
#' @return A `gp3bayes_pupil_complexity_audit` object.
#' @export
audit_pupil_computational_budget <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_advanced_specification")) {
    stop("`x` must be an advanced pupil specification.", call. = FALSE)
  }
  data <- x$data
  m <- x$mapping
  n <- nrow(data)
  n_time <- length(unique(data[[m$time]][is.finite(data[[m$time]])]))
  n_condition <- if (is.null(m$condition)) 1L else length(unique(data[[m$condition]][!is.na(data[[m$condition]])]))
  n_participant <- length(unique(data[[m$participant]][!is.na(data[[m$participant]])]))
  n_series <- if (is.null(m$trial)) n_participant else length(unique(interaction(data[[m$participant]], data[[m$trial]], drop = TRUE)))

  checks <- data.frame(
    check = character(), status = character(), message = character(), stringsAsFactors = FALSE
  )
  add <- function(check, status, message) {
    checks <<- rbind(checks, data.frame(check = check, status = status, message = message, stringsAsFactors = FALSE))
  }

  add("rows", if (n > 250000) "high" else if (n > 75000) "review" else "ok", paste0(n, " analysis rows"))
  add("series", if (n_series > 5000) "review" else "ok", paste0(n_series, " participant/trial series"))

  if (x$temporal_structure == "gaussian_process") {
    if (x$gp_spec$basis == "exact") {
      effective_points <- n_time * n_condition
      add(
        "exact_gp",
        if (effective_points > 500) "high" else if (effective_points > 250) "review" else "ok",
        paste0("exact GP across approximately ", effective_points, " unique time-by-condition locations")
      )
    } else {
      add(
        "approximate_gp",
        if (x$gp_spec$k > 100) "review" else "ok",
        paste0("approximate GP with k = ", x$gp_spec$k)
      )
    }
  }

  if (!is.null(x$autocorrelation)) {
    order_sum <- x$autocorrelation$p + x$autocorrelation$q
    add("arma_order", if (order_sum >= 4) "review" else "ok", paste0("ARMA order (", x$autocorrelation$p, ",", x$autocorrelation$q, ")"))
  }

  layered <- sum(
    x$temporal_structure == "gaussian_process",
    x$residual_scale == "condition_time",
    x$participant_trajectory == "factor_smooth",
    !is.null(x$autocorrelation),
    !is.null(x$measurement_model),
    !is.null(x$missingness_model)
  )
  add(
    "layered_complexity",
    if (layered >= 5) "high" else if (layered >= 3) "review" else "ok",
    paste0(layered, " advanced complexity layers requested simultaneously")
  )

  overall <- if (any(checks$status == "high")) "high" else if (any(checks$status == "review")) "review" else "ok"

  structure(
    list(
      overall_status = overall,
      rows = n,
      unique_time = n_time,
      conditions = n_condition,
      participants = n_participant,
      series = n_series,
      checks = checks
    ),
    class = "gp3bayes_pupil_complexity_audit"
  )
}

#' Tabulate an advanced pupil model specification
#'
#' @param x An advanced specification.
#' @return A one-row data frame.
#' @export
pupil_advanced_specification_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced pupil specification.", call. = FALSE)
  ac <- x$autocorrelation
  data.frame(
    version = x$version,
    family = x$family,
    temporal_structure = x$temporal_structure,
    residual_scale = x$residual_scale,
    gp_kernel = if (is.null(x$gp_spec)) NA_character_ else x$gp_spec$kernel,
    gp_basis = if (is.null(x$gp_spec)) NA_character_ else x$gp_spec$basis,
    gp_k = if (is.null(x$gp_spec)) NA_integer_ else x$gp_spec$k,
    arma_p = if (is.null(ac)) 0L else ac$p,
    arma_q = if (is.null(ac)) 0L else ac$q,
    participant_trajectory = x$participant_trajectory,
    item_effects = x$item_effects,
    measurement_model = !is.null(x$measurement_model),
    missingness_model = !is.null(x$missingness_model),
    predictive_target = x$predictive_target,
    complexity_status = x$complexity_audit$overall_status,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
print.gp3bayes_pupil_advanced_specification <- function(x, ...) {
  tab <- pupil_advanced_specification_table(x)
  cat("<gp3bayes_pupil_advanced_specification>\n")
  cat("  Version:", tab$version, "\n")
  cat("  Family:", tab$family, "\n")
  cat("  Temporal structure:", tab$temporal_structure, "\n")
  cat("  Residual scale:", tab$residual_scale, "\n")
  if (!is.na(tab$gp_kernel)) cat("  GP:", tab$gp_kernel, "/", tab$gp_basis, "\n")
  cat("  ARMA:", paste0("(", tab$arma_p, ",", tab$arma_q, ")"), "\n")
  cat("  Predictive target:", tab$predictive_target, "\n")
  cat("  Complexity:", tab$complexity_status, "\n")
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_advanced_specification <- function(x, row.names = NULL, optional = FALSE, ...) {
  pupil_advanced_specification_table(x)
}

#' @export
print.gp3bayes_pupil_complexity_audit <- function(x, ...) {
  cat("<gp3bayes_pupil_complexity_audit>\n")
  cat("  Status:", x$overall_status, "\n")
  cat("  Rows:", x$rows, "\n")
  cat("  Participants:", x$participants, "\n")
  cat("  Series:", x$series, "\n")
  print(x$checks, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_complexity_audit <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$checks
}

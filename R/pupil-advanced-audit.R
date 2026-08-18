# Identifiability, design-support, and predictive-calibration audits for gp3bayes 0.5.

.p05_audit_row <- function(domain, check, value, status, guidance) {
  data.frame(
    domain = domain,
    check = check,
    value = as.character(value),
    status = status,
    guidance = guidance,
    stringsAsFactors = FALSE
  )
}

#' Audit empirical support for an advanced pupil model specification
#'
#' This audit is deliberately heuristic. It identifies weakly supported design
#' configurations before sampling but does not certify model identifiability or
#' posterior adequacy.
#'
#' @param specification An advanced pupil specification.
#' @return A `gp3bayes_pupil_identifiability_audit` object.
#' @export
audit_advanced_pupil_identifiability <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced pupil specification.", call. = FALSE)
  spec <- specification
  data <- spec$data
  m <- spec$mapping
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- .p05_audit_row(...)

  n <- nrow(data)
  participants <- length(unique(data[[m$participant]][!is.na(data[[m$participant]])]))
  add("design", "rows", n, if (n >= 200L) "pass" else "review", "Very small time-course datasets may weakly identify flexible temporal models.")
  add("design", "participants", participants, if (participants >= 10L) "pass" else "review", "Few participants limit hierarchical variance estimation and new-participant generalization.")

  if (!is.null(m$condition)) {
    counts <- table(data[[m$condition]], useNA = "no")
    min_condition <- if (length(counts)) min(counts) else 0L
    add("design", "minimum_condition_rows", min_condition, if (min_condition >= 50L) "pass" else "review", "Inspect condition imbalance and time support; row count alone does not establish effective information.")
  }

  series <- .p05_series_data(data, m)
  lens <- table(series$.gp3bayes_series)
  min_len <- if (length(lens)) min(lens) else 0L
  med_len <- if (length(lens)) stats::median(as.numeric(lens)) else 0
  add("temporal", "minimum_series_length", min_len, if (min_len >= 8L) "pass" else "review", "Short series provide weak information about residual temporal dependence.")
  add("temporal", "median_series_length", med_len, if (med_len >= 12L) "pass" else "review", "Longer repeated series are generally needed as ARMA order increases.")

  if (!is.null(spec$autocorrelation)) {
    required <- max(8L, 5L * (spec$autocorrelation$p + spec$autocorrelation$q + 1L))
    add(
      "temporal", "arma_series_support",
      paste0("median=", med_len, "; recommended>=", required),
      if (med_len >= required) "pass" else "review",
      "The threshold is a conservative gp3bayes governance heuristic, not a theorem of ARMA identifiability."
    )
  }

  unique_time <- length(unique(data[[m$time]][is.finite(data[[m$time]])]))
  add("trajectory", "unique_time_points", unique_time, if (unique_time >= 10L) "pass" else "review", "Flexible smooth/GP trajectories require adequate distinct time support.")
  if (spec$temporal_structure == "gaussian_process") {
    gp <- spec$gp_spec
    if (gp$basis == "approximate") {
      add("trajectory", "gp_basis_rank", gp$k, if (gp$k <= unique_time && gp$k >= 8L) "pass" else "review", "Approximate GP basis dimension should be materially smaller than or compatible with unique temporal support.")
    } else {
      add("trajectory", "gp_exact_unique_time", unique_time, if (unique_time <= 300L) "pass" else "review", "Exact GP cost grows rapidly with the number of unique predictor locations; consider an approximate basis after scientific review.")
    }
  }

  y <- data[[m$response]]
  miss_y <- mean(!is.finite(y))
  add("missingness", "response_missing_fraction", signif(miss_y, 4), if (miss_y <= 0.10) "pass" else if (miss_y <= 0.30) "review" else "high", "Missingness rate is descriptive and does not identify the missingness mechanism.")
  if (!is.null(spec$missingness_model) && spec$missingness_model$response == "model") {
    add("missingness", "response_missingness_assumption", spec$missingness_model$assumption, "review", "The declared MAR assumption is an analysis assumption, not an empirical finding.")
  }

  mm <- spec$measurement_model
  if (!is.null(mm)) {
    mmap <- .p05_measurement_map(spec, data)
    for (v in names(mmap)) {
      se <- data[[mmap[[v]]]]
      x <- data[[v]]
      ratio <- stats::median(se[is.finite(se) & se >= 0], na.rm = TRUE) / .p05_safe_sd(x)
      add("measurement", paste0("median_se_to_sd_", v), signif(ratio, 4), if (is.finite(ratio) && ratio <= 1) "pass" else "review", "Large known measurement error relative to empirical predictor variation can make latent predictor effects weakly identified.")
    }
    if (!is.null(mm$response_error)) {
      se <- data[[mm$response_error]]
      ratio <- stats::median(se[is.finite(se) & se > 0], na.rm = TRUE) / .p05_safe_sd(y)
      add("measurement", "median_response_se_to_sd", signif(ratio, 4), if (is.finite(ratio) && ratio <= 1) "pass" else "review", "Known response uncertainty approaching or exceeding observed response variation warrants strong prior and sensitivity scrutiny.")
    }
  }

  if (spec$residual_scale != "constant") {
    add("distribution", "distributional_sigma", spec$residual_scale, if (n >= 500L) "pass" else "review", "Distributional sigma adds parameters and should be supported by enough observations over the relevant time/condition design.")
  }
  if (spec$family == "student") {
    add("distribution", "student_degrees_of_freedom", "estimated", "review", "Inspect posterior degrees of freedom and posterior predictive tails; Student-t is not a substitute for data-quality auditing.")
  }

  tab <- do.call(rbind, rows)
  rank <- c(pass = 0L, review = 1L, high = 2L)
  max_rank <- max(unname(rank[tab$status]))
  overall <- names(rank)[match(max_rank, rank)]

  structure(
    list(
      table = tab,
      overall = overall,
      certification = FALSE,
      specification = spec,
      interpretation = paste(
        "This audit screens design support and computational/model complexity before fitting.",
        "It does not prove statistical identifiability, convergence, causal validity, or substantive adequacy."
      )
    ),
    class = "gp3bayes_pupil_identifiability_audit"
  )
}

#' Tabulate advanced pupil identifiability/design-support audit
#' @param x An identifiability audit.
#' @return A data frame.
#' @export
pupil_identifiability_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_identifiability_audit")) stop("Expected an identifiability audit.", call. = FALSE)
  x$table
}

#' Score posterior predictive draws against observed pupil values
#'
#' Metrics are descriptive out-of-sample or held-out scores only when the caller
#' supplies predictions generated without using the scored observations.
#'
#' @param observed Numeric observed values.
#' @param draws Matrix of posterior predictive draws (draws x observations).
#' @param probability Central interval used for empirical coverage and width.
#' @return A `gp3bayes_pupil_predictive_score` object.
#' @export
score_pupil_predictions <- function(observed, draws, probability = 0.90) {
  if (!is.numeric(observed)) stop("`observed` must be numeric.", call. = FALSE)
  if (!is.matrix(draws)) draws <- as.matrix(draws)
  if (ncol(draws) != length(observed)) stop("Prediction columns must equal length(observed).", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  ok <- is.finite(observed)
  if (!any(ok)) stop("No finite observed outcomes are available for scoring.", call. = FALSE)
  observed <- observed[ok]
  draws <- draws[, ok, drop = FALSE]
  pred_mean <- colMeans(draws)
  pred_median <- apply(draws, 2L, stats::median)
  alpha <- (1 - probability) / 2
  lo <- apply(draws, 2L, stats::quantile, probs = alpha, names = FALSE)
  hi <- apply(draws, 2L, stats::quantile, probs = 1 - alpha, names = FALSE)
  rmse <- sqrt(mean((pred_mean - observed)^2))
  mae <- mean(abs(pred_median - observed))
  coverage <- mean(observed >= lo & observed <= hi)
  width <- mean(hi - lo)
  bias <- mean(pred_mean - observed)
  # Empirical-draw CRPS identity: E|X-y| - .5 E|X-X'|.  The
  # pairwise term is computed exactly from sorted draws in O(S log S) per
  # observation rather than materialising an S x S distance matrix.
  first <- colMeans(abs(sweep(draws, 2L, observed, "-")))
  if (nrow(draws) >= 2L) {
    S <- nrow(draws)
    weights <- 2 * seq_len(S) - S - 1
    half_pairwise <- apply(
      draws,
      2L,
      function(z) sum(weights * sort(z)) / (S^2)
    )
    crps_point <- first - half_pairwise
    crps <- mean(crps_point)
  } else {
    crps_point <- rep(NA_real_, length(observed))
    crps <- NA_real_
  }
  tab <- data.frame(
    metric = c("rmse_posterior_mean", "mae_posterior_median", "mean_bias", "interval_coverage", "mean_interval_width", "approx_crps"),
    value = c(rmse, mae, bias, coverage, width, crps),
    probability = c(NA, NA, NA, probability, probability, NA),
    stringsAsFactors = FALSE
  )
  pointwise <- data.frame(
    observed = observed,
    predicted_mean = pred_mean,
    predicted_median = pred_median,
    q_low = lo,
    q_high = hi,
    covered = observed >= lo & observed <= hi,
    crps = crps_point,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      table = tab,
      pointwise = pointwise,
      probability = probability,
      interpretation = "Predictive scores quantify calibration/error for the supplied prediction task; they do not establish causal or substantive model adequacy."
    ),
    class = "gp3bayes_pupil_predictive_score"
  )
}

#' Audit posterior predictive calibration on explicit evaluation data
#'
#' @param fit An advanced fitted model.
#' @param newdata Evaluation data containing the pupil response.
#' @param ndraws Number of posterior predictive draws.
#' @param probability Interval probability.
#' @param population_only Exclude group-level effects if TRUE.
#' @param allow_new_levels Passed to brms prediction.
#' @return A predictive-score object with evaluation metadata.
#' @export
audit_pupil_predictive_calibration <- function(
    fit,
    newdata,
    ndraws = 500L,
    probability = 0.90,
    population_only = FALSE,
    allow_new_levels = FALSE) {
  if (!inherits(fit, "gp3bayes_pupil_advanced_fit")) stop("Expected an advanced pupil fit.", call. = FALSE)
  if (!is.data.frame(newdata)) stop("`newdata` must be an explicit evaluation data.frame.", call. = FALSE)
  .p05_assert_flag(population_only, "population_only")
  .p05_assert_flag(allow_new_levels, "allow_new_levels")
  .p05_assert_probability(probability, "probability")
  response <- fit$specification$mapping$response
  if (!response %in% names(newdata)) stop("Evaluation data must contain the pupil response column.", call. = FALSE)
  ndraws <- as.integer(ndraws)
  if (ndraws < 20L) stop("`ndraws` must be at least 20 for calibration summaries.", call. = FALSE)
  re_formula <- if (population_only) NA else NULL
  draws <- brms::posterior_predict(
    .p05_underlying_fit(fit),
    newdata = newdata,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels
  )
  if (length(dim(draws)) != 2L) stop("Calibration currently requires a univariate posterior predictive matrix.", call. = FALSE)
  out <- score_pupil_predictions(newdata[[response]], draws, probability)
  out$evaluation_rows <- nrow(newdata)
  out$population_only <- population_only
  out$allow_new_levels <- allow_new_levels
  class(out) <- c("gp3bayes_pupil_predictive_calibration", class(out))
  out
}

#' @export
print.gp3bayes_pupil_identifiability_audit <- function(x, ...) {
  cat("<gp3bayes_pupil_identifiability_audit>\n")
  cat("  Overall:", x$overall, "\n")
  cat("  Certification: FALSE\n")
  print(x$table[, c("domain", "check", "value", "status")], row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_identifiability_audit <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_predictive_score <- function(x, ...) {
  cat("<gp3bayes_pupil_predictive_score>\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_predictive_score <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_predictive_calibration <- function(x, ...) {
  cat("<gp3bayes_pupil_predictive_calibration>\n")
  cat("  Evaluation rows:", x$evaluation_rows, "\n")
  NextMethod("print")
  invisible(x)
}

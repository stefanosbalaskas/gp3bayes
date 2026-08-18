# Posterior predictions and estimands for gp3bayes 0.5 advanced pupil models.

.p05_reference_value <- function(x) {
  if (is.numeric(x)) {
    z <- stats::median(x, na.rm = TRUE)
    if (!is.finite(z)) return(NA_real_)
    return(z)
  }
  if (is.factor(x)) {
    lev <- levels(x)
    if (!length(lev)) return(x[NA_integer_])
    return(factor(lev[[1L]], levels = lev, ordered = is.ordered(x)))
  }
  z <- x[!is.na(x)]
  if (length(z)) z[[1L]] else NA
}

.p05_prediction_grid <- function(fit, newdata = NULL, max_grid = 5000L) {
  spec <- .p05_fit_spec(fit)
  if (is.null(spec)) stop("A gp3bayes 0.5 advanced fit is required.", call. = FALSE)
  max_grid <- .p05_assert_integerish(max_grid, "max_grid", 1L, 1000000L)
  data <- fit$translation$data
  m <- spec$mapping
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) stop("`newdata` must be a data.frame.", call. = FALSE)
    grid <- newdata
    if (nrow(grid) > max_grid) stop("`newdata` exceeds `max_grid`.", call. = FALSE)
    return(grid)
  }

  times <- sort(unique(data[[m$time]][is.finite(data[[m$time]])]))
  if (length(times) > 200L) {
    times <- unique(as.numeric(stats::quantile(times, probs = seq(0, 1, length.out = 200), names = FALSE)))
  }
  if (is.null(m$condition)) {
    grid <- data.frame(times, check.names = FALSE)
    names(grid) <- m$time
  } else {
    cond <- unique(data[[m$condition]])
    cond <- cond[!is.na(cond)]
    grid <- expand.grid(
      .time = times,
      .condition = cond,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    names(grid) <- c(m$time, m$condition)
    if (is.factor(data[[m$condition]])) {
      grid[[m$condition]] <- factor(grid[[m$condition]], levels = levels(data[[m$condition]]))
    }
  }

  # Hold ordinary covariates and missingness-model auxiliary predictors at
  # transparent reference values. Auxiliary variables can appear in latent
  # predictor equations even when they are not part of the primary mean model.
  reference_vars <- spec$covariates
  if (!is.null(spec$missingness_model)) {
    reference_vars <- unique(c(reference_vars, spec$missingness_model$auxiliary_predictors))
  }
  for (v in reference_vars) {
    if (!v %in% names(grid)) grid[[v]] <- .p05_reference_value(data[[v]])
  }

  # Newdata for measurement-error models must also carry the declared known
  # standard-error columns. For generated population grids use their observed
  # median as an explicit reference uncertainty; user-supplied newdata is never
  # silently amended here and is validated by brms during prediction.
  if (!is.null(spec$measurement_model)) {
    se_vars <- c(unname(spec$measurement_model$covariate_errors), spec$measurement_model$response_error)
    se_vars <- unique(se_vars[!is.na(se_vars) & nzchar(se_vars)])
    for (v in se_vars) {
      if (!v %in% names(grid)) grid[[v]] <- .p05_reference_value(data[[v]])
    }
  }

  # Required grouping variables are populated with existing reference levels.
  p <- m$participant
  grid[[p]] <- data[[p]][which(!is.na(data[[p]]))[[1L]]]
  if (is.factor(data[[p]])) grid[[p]] <- factor(grid[[p]], levels = levels(data[[p]]))
  if (!is.null(m$item) && spec$item_effects) {
    grid[[m$item]] <- data[[m$item]][which(!is.na(data[[m$item]]))[[1L]]]
    if (is.factor(data[[m$item]])) grid[[m$item]] <- factor(grid[[m$item]], levels = levels(data[[m$item]]))
  }
  if (!is.null(m$trial)) {
    grid[[m$trial]] <- data[[m$trial]][which(!is.na(data[[m$trial]]))[[1L]]]
  }

  if (nrow(grid) > max_grid) stop("Prediction grid exceeds `max_grid`.", call. = FALSE)
  grid
}

#' Predict an advanced pupil trajectory
#'
#' @param fit A fitted advanced pupil model.
#' @param newdata Optional prediction data. If omitted, a population-level
#'   time-by-condition grid is generated with covariates held at reference
#'   values.
#' @param type `"expected"`, `"posterior_predictive"`, or `"linear"`.
#' @param ndraws Number of posterior draws.
#' @param population_only Exclude group-level effects when TRUE.
#' @param allow_new_levels Passed to brms prediction methods.
#' @param max_grid Maximum generated grid size.
#' @return A `gp3bayes_pupil_advanced_trajectory` object.
#' @export
predict_advanced_pupil_trajectory <- function(
    fit,
    newdata = NULL,
    type = c("expected", "posterior_predictive", "linear"),
    ndraws = 500L,
    population_only = TRUE,
    allow_new_levels = FALSE,
    max_grid = 5000L) {

  type <- match.arg(type)
  .p05_assert_flag(population_only, "population_only")
  .p05_assert_flag(allow_new_levels, "allow_new_levels")
  ndraws <- .p05_assert_integerish(ndraws, "ndraws", 1L, 100000L)
  bfit <- .p05_underlying_fit(fit)
  spec <- .p05_fit_spec(fit)

  if (!is.null(spec$autocorrelation) && !population_only && is.null(newdata)) {
    stop(
      "ARMA-aware conditional prediction requires explicit series-aware `newdata`; use `population_only = TRUE` for marginal trajectory summaries.",
      call. = FALSE
    )
  }

  grid <- .p05_prediction_grid(fit, newdata, max_grid)
  if (!is.null(spec$autocorrelation) && !population_only) {
    needed <- unique(c(spec$mapping$participant, spec$mapping$trial, spec$mapping$time))
    needed <- needed[!vapply(needed, is.null, logical(1L))]
    missing_needed <- setdiff(needed, names(grid))
    if (length(missing_needed)) {
      stop("ARMA-aware `newdata` is missing series column(s): ", paste(missing_needed, collapse = ", "), ".", call. = FALSE)
    }
    grid <- .p05_series_data(grid, spec$mapping)
  }

  re_formula <- if (population_only) NA else NULL
  incl_autocor <- !population_only && !is.null(spec$autocorrelation)
  common <- list(
    object = bfit,
    newdata = grid,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    resp = spec$mapping$response,
    incl_autocor = incl_autocor
  )

  draws <- switch(
    type,
    expected = do.call(brms::posterior_epred, common),
    posterior_predictive = do.call(brms::posterior_predict, common),
    linear = do.call(
      brms::posterior_linpred,
      c(common, list(transform = FALSE))
    )
  )

  if (length(dim(draws)) != 2L) {
    stop("Advanced univariate trajectory prediction expected a draws-by-observations matrix.", call. = FALSE)
  }

  structure(
    list(
      grid = grid,
      draws = draws,
      type = type,
      population_only = population_only,
      specification = fit$specification
    ),
    class = c("gp3bayes_pupil_advanced_trajectory", "gp3bayes_pupil_trajectory")
  )
}

#' Summarise an advanced pupil trajectory
#'
#' @param prediction An advanced trajectory prediction.
#' @param probability Central posterior interval probability.
#' @return A data frame.
#' @export
advanced_pupil_trajectory_table <- function(prediction, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_advanced_trajectory")) stop("Expected an advanced trajectory prediction.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  alpha <- (1 - probability) / 2
  s <- .p05_quantile_summary(prediction$draws, c(alpha, 0.5, 1 - alpha))
  cbind(prediction$grid, s, row.names = NULL)
}

#' Estimate the residual-scale trajectory from a distributional model
#'
#' @param fit An advanced fit.
#' @param newdata Optional prediction data.
#' @param ndraws Number of posterior draws.
#' @param probability Central interval probability.
#' @return A `gp3bayes_pupil_residual_scale` object.
#' @export
estimate_pupil_residual_scale <- function(
    fit,
    newdata = NULL,
    ndraws = 500L,
    probability = 0.95) {

  spec <- .p05_fit_spec(fit)
  if (is.null(spec)) stop("Expected an advanced fit.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  ndraws <- .p05_assert_integerish(ndraws, "ndraws", 1L, 100000L)
  bfit <- .p05_underlying_fit(fit)
  grid <- .p05_prediction_grid(fit, newdata)

  # Constant sigma is still a valid estimand; dpar prediction repeats it over
  # rows. transform=TRUE returns response-scale sigma.
  draws <- tryCatch(
    brms::posterior_linpred(
      bfit,
      newdata = grid,
      dpar = "sigma",
      resp = spec$mapping$response,
      transform = TRUE,
      ndraws = ndraws,
      re_formula = NA,
      incl_autocor = FALSE
    ),
    error = function(e) {
      stop("Could not obtain posterior sigma draws from this fit: ", conditionMessage(e), call. = FALSE)
    }
  )

  structure(
    list(
      grid = grid,
      draws = draws,
      probability = probability,
      residual_scale = spec$residual_scale,
      specification = spec
    ),
    class = "gp3bayes_pupil_residual_scale"
  )
}

#' Tabulate posterior residual scale
#' @param x A residual-scale estimand.
#' @return A data frame.
#' @export
pupil_residual_scale_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_residual_scale")) stop("Expected a pupil residual-scale estimand.", call. = FALSE)
  alpha <- (1 - x$probability) / 2
  cbind(x$grid, .p05_quantile_summary(x$draws, c(alpha, 0.5, 1 - alpha)), row.names = NULL)
}

#' Extract Gaussian-process hyperparameters
#'
#' @param fit A fitted GP pupil model.
#' @param probability Central interval probability.
#' @return A `gp3bayes_pupil_gp_hyperparameters` object.
#' @export
pupil_gp_hyperparameters <- function(fit, probability = 0.95) {
  spec <- .p05_fit_spec(fit)
  if (is.null(spec) || spec$temporal_structure != "gaussian_process") {
    stop("`fit` must come from a Gaussian-process advanced pupil specification.", call. = FALSE)
  }
  .p05_assert_probability(probability, "probability")
  .p05_require("posterior", "to extract GP hyperparameters")
  draws <- posterior::as_draws_df(.p05_underlying_fit(fit))
  nms <- names(draws)
  keep <- grepl("(^|_)sdgp|(^|_)lscale", nms)
  vars <- nms[keep]
  if (!length(vars)) stop("No `sdgp` or `lscale` parameters were found in posterior draws.", call. = FALSE)
  mat <- posterior::as_draws_matrix(posterior::subset_draws(draws, variable = vars))
  alpha <- (1 - probability) / 2
  tab <- .p05_quantile_summary(mat, c(alpha, 0.5, 1 - alpha))
  tab$parameter <- vars
  tab$type <- ifelse(grepl("lscale", vars), "length_scale", "marginal_sd")
  tab <- tab[, c("parameter", "type", "mean", "sd", "q_low", "median", "q_high")]

  structure(
    list(table = tab, probability = probability, gp_spec = spec$gp_spec),
    class = "gp3bayes_pupil_gp_hyperparameters"
  )
}

#' Tabulate GP hyperparameters
#' @param x A GP-hyperparameter object.
#' @return A data frame.
#' @export
pupil_gp_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_gp_hyperparameters")) stop("Expected GP hyperparameters.", call. = FALSE)
  x$table
}

#' Summarise measurement uncertainty declared in a model
#'
#' @param x A measurement model, advanced specification, or advanced fit.
#' @return A data frame.
#' @export
pupil_measurement_uncertainty_table <- function(x) {
  mm <- if (inherits(x, "gp3bayes_pupil_measurement_model")) x else if (inherits(x, "gp3bayes_pupil_advanced_specification")) x$measurement_model else if (inherits(x, "gp3bayes_pupil_advanced_fit")) x$specification$measurement_model else NULL
  if (is.null(mm)) return(data.frame(variable = character(), error_column = character(), role = character(), stringsAsFactors = FALSE))
  cov_errors <- mm$covariate_errors

  if (length(cov_errors)) {

    cov_names <- names(cov_errors)

    if (
      is.null(cov_names) ||
      length(cov_names) != length(cov_errors) ||
      anyNA(cov_names) ||
      any(!nzchar(cov_names))
    ) {
      stop(
        paste(
          "Malformed measurement model: predictor-error declarations",
          "must retain one non-empty covariate name per error column."
        ),
        call. = FALSE
      )
    }

    out <- data.frame(
      variable = cov_names,
      error_column = unname(cov_errors),
      role = rep("predictor", length(cov_errors)),
      stringsAsFactors = FALSE
    )

  } else {

    out <- data.frame(
      variable = character(),
      error_column = character(),
      role = character(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(mm$response_error)) {
    out <- rbind(out, data.frame(variable = "<pupil response>", error_column = mm$response_error, role = "response", stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out
}

#' Audit a declared measurement model against data
#'
#' @param specification An advanced specification containing a measurement model.
#' @return A `gp3bayes_pupil_measurement_audit_05` object.
#' @export
audit_pupil_measurement_model <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced specification.", call. = FALSE)
  mm <- specification$measurement_model
  if (is.null(mm)) stop("No measurement model is declared.", call. = FALSE)
  data <- specification$data
  tab <- pupil_measurement_uncertainty_table(mm)
  checks <- lapply(seq_len(nrow(tab)), function(i) {
    col <- tab$error_column[[i]]
    x <- data[[col]]
    data.frame(
      variable = tab$variable[[i]],
      error_column = col,
      role = tab$role[[i]],
      missing_fraction = mean(is.na(x)),
      nonpositive_fraction = mean(!is.na(x) & (!is.finite(x) | x <= 0)),
      status = if (all(is.na(x)) || any(!is.na(x) & (!is.finite(x) | x <= 0))) "failure" else if (anyNA(x)) "review" else "pass",
      stringsAsFactors = FALSE
    )
  })
  checks <- do.call(rbind, checks)
  structure(
    list(
      table = checks,
      status = if (any(checks$status == "failure")) "failure" else if (any(checks$status == "review")) "review" else "pass",
      interpretation = "Known standard errors are treated as declared measurement uncertainty; this audit does not validate calibration or unbiasedness."
    ),
    class = "gp3bayes_pupil_measurement_audit_05"
  )
}

#' Audit missingness in an advanced pupil specification
#'
#' @param specification An advanced specification.
#' @return A `gp3bayes_pupil_missingness_audit` object.
#' @export
audit_pupil_missingness <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced specification.", call. = FALSE)
  ms <- specification$missingness_model
  if (is.null(ms)) stop("No missingness model is declared.", call. = FALSE)
  data <- specification$data
  vars <- unique(c(specification$mapping$response, ms$predictors, ms$auxiliary_predictors))
  tab <- data.frame(
    variable = vars,
    n = vapply(vars, function(v) length(data[[v]]), integer(1L)),
    missing = vapply(vars, function(v) sum(is.na(data[[v]])), integer(1L)),
    missing_fraction = vapply(vars, function(v) mean(is.na(data[[v]])), numeric(1L)),
    stringsAsFactors = FALSE
  )
  tab$role <- ifelse(tab$variable == specification$mapping$response, "response", ifelse(tab$variable %in% ms$predictors, "modelled_predictor", "auxiliary"))

  # Time-localized missingness is reported descriptively without claiming a
  # missingness mechanism. Degenerate/short time vectors fall back to a single
  # explicit bin rather than failing inside cut().
  t <- data[[specification$mapping$time]]
  finite_t <- t[is.finite(t)]
  breaks <- if (length(finite_t)) {
    unique(stats::quantile(finite_t, probs = seq(0, 1, length.out = 6), na.rm = TRUE, names = FALSE))
  } else {
    numeric()
  }
  bins <- if (length(breaks) >= 2L) {
    cut(t, breaks = breaks, include.lowest = TRUE)
  } else {
    factor(rep("all_times", length(t)), levels = "all_times")
  }
  resp_miss <- is.na(data[[specification$mapping$response]])
  by_time <- stats::aggregate(resp_miss, list(time_bin = bins), mean, na.rm = TRUE)
  names(by_time)[[2L]] <- "response_missing_fraction"

  structure(
    list(
      table = tab,
      by_time = by_time,
      assumptions = ms$assumptions,
      interpretation = "Observed missingness patterns are descriptive. The MAR declaration is an assumption, not an empirical conclusion."
    ),
    class = "gp3bayes_pupil_missingness_audit"
  )
}

#' Tabulate missingness audit results
#' @param x A missingness audit.
#' @return A data frame.
#' @export
pupil_missingness_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_missingness_audit")) stop("Expected a missingness audit.", call. = FALSE)
  x$table
}

#' @export
print.gp3bayes_pupil_advanced_trajectory <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_trajectory>\n")
  cat("  Grid rows:", nrow(x$grid), "\n")
  cat("  Draws:", nrow(x$draws), "\n")
  cat("  Type:", x$type, "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_advanced_trajectory <- function(x, row.names = NULL, optional = FALSE, ...) advanced_pupil_trajectory_table(x)

#' @export
print.gp3bayes_pupil_residual_scale <- function(x, ...) {
  cat("<gp3bayes_pupil_residual_scale>\n")
  cat("  Model:", x$residual_scale, "\n")
  cat("  Grid rows:", nrow(x$grid), "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_residual_scale <- function(x, row.names = NULL, optional = FALSE, ...) pupil_residual_scale_table(x)

#' @export
print.gp3bayes_pupil_gp_hyperparameters <- function(x, ...) {
  cat("<gp3bayes_pupil_gp_hyperparameters>\n")
  cat("  Kernel:", x$gp_spec$kernel, "\n")
  cat("  Parameters:", nrow(x$table), "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_gp_hyperparameters <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_missingness_audit <- function(x, ...) {
  cat("<gp3bayes_pupil_missingness_audit>\n")
  cat("  Assumption:", x$assumptions, "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_missingness_audit <- function(x, row.names = NULL, optional = FALSE, ...) x$table

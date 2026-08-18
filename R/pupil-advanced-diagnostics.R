# Diagnostics for gp3bayes 0.5 advanced pupil models.

#' Audit empirical temporal dependence before model fitting
#'
#' Computes descriptive within-series autocorrelation and spacing diagnostics.
#' The audit is diagnostic only and does not select an ARMA order.
#'
#' @param x A prepared pupil object, data frame, or advanced specification.
#' @param max_lag Maximum lag for descriptive ACF summaries.
#' @return A `gp3bayes_pupil_temporal_dependence_audit` object.
#' @export
audit_pupil_temporal_dependence <- function(x, max_lag = 10L) {
  spec <- if (inherits(x, "gp3bayes_pupil_advanced_specification")) x else NULL
  prepared <- if (is.null(spec)) x else spec$prepared
  data <- if (is.null(spec)) .p05_data(prepared) else spec$data
  mapping <- if (is.null(spec)) .p05_mapping(prepared, require_condition = FALSE) else spec$mapping
  max_lag <- as.integer(max_lag)
  if (max_lag < 1L || max_lag > 100L) stop("`max_lag` must be 1--100.", call. = FALSE)

  d <- .p05_series_data(data, mapping)
  y <- mapping$response
  t <- mapping$time
  groups <- split(seq_len(nrow(d)), d$.gp3bayes_series)

  rows <- lapply(names(groups), function(g) {
    ii <- groups[[g]]
    yy <- d[[y]][ii]
    tt <- d[[t]][ii]
    ok <- is.finite(yy) & is.finite(tt)
    yy <- yy[ok]
    tt <- tt[ok]
    n <- length(yy)
    if (n < 4L) {
      return(data.frame(series = g, n = n, lag1 = NA_real_, lag2 = NA_real_, median_step = NA_real_, irregularity = NA_real_))
    }
    ac <- stats::acf(yy, lag.max = min(max_lag, n - 1L), plot = FALSE, na.action = stats::na.pass)$acf
    dt <- diff(sort(tt))
    med_dt <- stats::median(dt, na.rm = TRUE)
    irr <- if (!is.finite(med_dt) || med_dt == 0) NA_real_ else stats::mad(dt, constant = 1, na.rm = TRUE) / abs(med_dt)
    data.frame(
      series = g,
      n = n,
      lag1 = if (length(ac) >= 2L) ac[[2L]] else NA_real_,
      lag2 = if (length(ac) >= 3L) ac[[3L]] else NA_real_,
      median_step = med_dt,
      irregularity = irr,
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL

  summary <- data.frame(
    metric = c("series", "median_length", "median_lag1", "median_abs_lag1", "median_irregularity", "short_series_fraction"),
    value = c(
      nrow(tab),
      stats::median(tab$n, na.rm = TRUE),
      stats::median(tab$lag1, na.rm = TRUE),
      stats::median(abs(tab$lag1), na.rm = TRUE),
      stats::median(tab$irregularity, na.rm = TRUE),
      mean(tab$n < 6L)
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      series = tab,
      summary = summary,
      max_lag = max_lag,
      interpretation = paste(
        "Empirical autocorrelation describes the observed series and may reflect mean-trajectory misspecification, residual dependence, preprocessing, or design structure.",
        "It is not an automatic ARMA-order selector."
      )
    ),
    class = "gp3bayes_pupil_temporal_dependence_audit"
  )
}

#' Tabulate a temporal-dependence audit
#' @param x A temporal-dependence audit.
#' @param level `"summary"` or `"series"`.
#' @export
pupil_autocorrelation_table <- function(x, level = c("summary", "series")) {
  if (!inherits(x, "gp3bayes_pupil_temporal_dependence_audit")) stop("Expected a temporal-dependence audit.", call. = FALSE)
  level <- match.arg(level)
  x[[level]]
}

#' Compare residual autocorrelation across fitted advanced models
#'
#' This function does not choose a winner. It summarises lag-specific residual
#' dependence after subtracting posterior expected means.
#'
#' @param ... Two or more named advanced fits, or one named list.
#' @param max_lag Maximum residual ACF lag.
#' @param ndraws Draws used for posterior expected means.
#' @return A `gp3bayes_pupil_autocorrelation_comparison` object.
#' @export
compare_pupil_autocorrelation <- function(..., max_lag = 10L, ndraws = 300L) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !inherits(xs[[1L]], "gp3bayes_pupil_advanced_fit")) xs <- xs[[1L]]
  if (length(xs) < 2L) stop("Provide at least two fitted models.", call. = FALSE)
  if (is.null(names(xs)) || any(!nzchar(names(xs)))) names(xs) <- paste0("model", seq_along(xs))
  max_lag <- as.integer(max_lag)

  rows <- lapply(seq_along(xs), function(i) {
    fit <- xs[[i]]
    spec <- .p05_fit_spec(fit)
    if (is.null(spec)) stop("All models must be advanced pupil fits.", call. = FALSE)
    bfit <- .p05_underlying_fit(fit)
    data <- fit$translation$data
    ep <- brms::posterior_epred(bfit, newdata = data, ndraws = ndraws, re_formula = NULL)
    mu <- colMeans(ep)
    resid <- data[[spec$mapping$response]] - mu
    groups <- split(resid, data$.gp3bayes_series)
    acfs <- lapply(groups, function(z) {
      z <- z[is.finite(z)]
      if (length(z) < 4L) return(rep(NA_real_, max_lag))
      a <- stats::acf(z, lag.max = min(max_lag, length(z) - 1L), plot = FALSE)$acf[-1L]
      c(a, rep(NA_real_, max_lag - length(a)))[seq_len(max_lag)]
    })
    mat <- do.call(rbind, acfs)
    data.frame(
      model = names(xs)[[i]],
      lag = seq_len(max_lag),
      median_acf = apply(mat, 2L, stats::median, na.rm = TRUE),
      median_abs_acf = apply(abs(mat), 2L, stats::median, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL
  structure(
    list(table = tab, max_lag = max_lag, interpretation = "Residual ACF is a diagnostic comparison and does not establish a preferred model."),
    class = "gp3bayes_pupil_autocorrelation_comparison"
  )
}

#' Diagnose an advanced pupil fit
#'
#' @param fit An advanced pupil fit.
#' @param rhat_threshold Maximum preferred R-hat.
#' @param ess_threshold Minimum preferred bulk/tail ESS.
#' @return A `gp3bayes_pupil_advanced_diagnostics` object.
#' @export
diagnose_advanced_pupil_fit <- function(fit, rhat_threshold = 1.01, ess_threshold = 400) {
  .p05_require("posterior", "for advanced sampling diagnostics")
  bfit <- .p05_underlying_fit(fit)
  draws <- posterior::as_draws_array(bfit)
  sm <- posterior::summarise_draws(
    draws,
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )

  finite_rhat <- sm$rhat[is.finite(sm$rhat)]
  finite_bulk <- sm$ess_bulk[is.finite(sm$ess_bulk)]
  finite_tail <- sm$ess_tail[is.finite(sm$ess_tail)]

  nuts <- tryCatch(brms::nuts_params(bfit), error = function(e) NULL)
  divergences <- NA_integer_
  treedepth_hits <- NA_integer_
  if (!is.null(nuts) && nrow(nuts)) {
    if ("Parameter" %in% names(nuts) && "Value" %in% names(nuts)) {
      divergences <- sum(nuts$Parameter == "divergent__" & nuts$Value == 1)
      if ("treedepth__" %in% nuts$Parameter) {
        max_td <- fit$sampling$max_treedepth
        treedepth_hits <- sum(nuts$Parameter == "treedepth__" & nuts$Value >= max_td)
      }
    }
  }

  metrics <- data.frame(
    metric = c("max_rhat", "min_bulk_ess", "min_tail_ess", "divergences", "max_treedepth_hits"),
    value = c(
      if (length(finite_rhat)) max(finite_rhat) else NA_real_,
      if (length(finite_bulk)) min(finite_bulk) else NA_real_,
      if (length(finite_tail)) min(finite_tail) else NA_real_,
      divergences,
      treedepth_hits
    ),
    threshold = c(rhat_threshold, ess_threshold, ess_threshold, 0, 0),
    direction = c("<=", ">=", ">=", "=", "="),
    stringsAsFactors = FALSE
  )
  metrics$status <- c(
    ifelse(is.finite(metrics$value[[1L]]) && metrics$value[[1L]] <= rhat_threshold, "pass", "review"),
    ifelse(is.finite(metrics$value[[2L]]) && metrics$value[[2L]] >= ess_threshold, "pass", "review"),
    ifelse(is.finite(metrics$value[[3L]]) && metrics$value[[3L]] >= ess_threshold, "pass", "review"),
    ifelse(is.na(divergences), "unknown", ifelse(divergences == 0L, "pass", "review")),
    ifelse(is.na(treedepth_hits), "unknown", ifelse(treedepth_hits == 0L, "pass", "review"))
  )

  structure(
    list(
      metrics = metrics,
      parameter_summary = as.data.frame(sm),
      interpretation = "Threshold passes are numerical diagnostics, not automatic evidence of model adequacy or substantive validity."
    ),
    class = "gp3bayes_pupil_advanced_diagnostics"
  )
}

#' Compute a descriptive residual spectrum
#'
#' Provides a frequency-domain diagnostic for residual periodicity after
#' posterior mean subtraction. It does not infer physiological oscillations.
#'
#' @param fit An advanced fit.
#' @param ndraws Posterior expected-mean draws.
#' @return A `gp3bayes_pupil_residual_spectrum` object.
#' @export
pupil_residual_spectrum <- function(fit, ndraws = 300L) {
  spec <- .p05_fit_spec(fit)
  if (is.null(spec)) stop("Expected an advanced fit.", call. = FALSE)
  data <- fit$translation$data
  ep <- brms::posterior_epred(.p05_underlying_fit(fit), newdata = data, ndraws = ndraws, re_formula = NULL)
  resid <- data[[spec$mapping$response]] - colMeans(ep)
  groups <- split(resid, data$.gp3bayes_series)
  spectra <- lapply(groups, function(z) {
    z <- z[is.finite(z)]
    if (length(z) < 8L) return(NULL)
    sp <- stats::spectrum(z, plot = FALSE, detrend = TRUE, demean = TRUE)
    data.frame(frequency = sp$freq, power = sp$spec)
  })
  spectra <- spectra[!vapply(spectra, is.null, logical(1L))]
  if (!length(spectra)) stop("Too few complete residual observations for a residual spectrum.", call. = FALSE)
  # Interpolate each spectrum to a common normalized frequency grid.
  grid <- seq(0.01, 0.5, length.out = 100)
  mat <- vapply(
    spectra,
    function(z) stats::approx(z$frequency, z$power, xout = grid, rule = 2)$y,
    numeric(length(grid))
  )
  tab <- data.frame(
    frequency = grid,
    median_power = apply(mat, 1L, stats::median, na.rm = TRUE),
    q25_power = apply(mat, 1L, stats::quantile, probs = 0.25, na.rm = TRUE, names = FALSE),
    q75_power = apply(mat, 1L, stats::quantile, probs = 0.75, na.rm = TRUE, names = FALSE)
  )
  structure(
    list(table = tab, n_series = length(spectra), interpretation = "Residual spectral peaks are descriptive and must not be labelled as cognitive or physiological rhythms without an independent measurement model."),
    class = "gp3bayes_pupil_residual_spectrum"
  )
}

#' @export
print.gp3bayes_pupil_temporal_dependence_audit <- function(x, ...) {
  cat("<gp3bayes_pupil_temporal_dependence_audit>\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_temporal_dependence_audit <- function(x, row.names = NULL, optional = FALSE, ...) x$series

#' @export
print.gp3bayes_pupil_autocorrelation_comparison <- function(x, ...) {
  cat("<gp3bayes_pupil_autocorrelation_comparison>\n")
  cat("  Models:", paste(unique(x$table$model), collapse = ", "), "\n")
  cat("  Max lag:", x$max_lag, "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_autocorrelation_comparison <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_advanced_diagnostics <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_diagnostics>\n")
  print(x$metrics, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_advanced_diagnostics <- function(x, row.names = NULL, optional = FALSE, ...) x$metrics

#' @export
print.gp3bayes_pupil_residual_spectrum <- function(x, ...) {
  cat("<gp3bayes_pupil_residual_spectrum>\n")
  cat("  Series:", x$n_series, "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_residual_spectrum <- function(x, row.names = NULL, optional = FALSE, ...) x$table

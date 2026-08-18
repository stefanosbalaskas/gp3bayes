# Base-R graphics for gp3bayes 0.5 advanced pupillometry.

.p05_plot_interval <- function(x, mean, low, high, group = NULL, xlab = "Time", ylab = "Pupil", main = NULL, ...) {
  if (is.null(group)) {
    graphics::plot(x, mean, type = "l", xlab = xlab, ylab = ylab, main = main, ...)
    graphics::polygon(c(x, rev(x)), c(low, rev(high)), border = NA, col = grDevices::adjustcolor("grey70", alpha.f = 0.45))
    graphics::lines(x, mean, lwd = 2)
    return(invisible(NULL))
  }
  lev <- unique(group)
  yr <- range(c(low, high), finite = TRUE)
  xr <- range(x, finite = TRUE)
  graphics::plot(NA, xlim = xr, ylim = yr, xlab = xlab, ylab = ylab, main = main, ...)
  pal <- grDevices::hcl.colors(max(3L, length(lev)), palette = "Dark 3")
  for (i in seq_along(lev)) {
    ii <- which(group == lev[[i]])
    o <- order(x[ii])
    xx <- x[ii][o]
    mm <- mean[ii][o]
    ll <- low[ii][o]
    hh <- high[ii][o]
    graphics::polygon(c(xx, rev(xx)), c(ll, rev(hh)), border = NA, col = grDevices::adjustcolor(pal[[i]], alpha.f = 0.18))
    graphics::lines(xx, mm, lwd = 2, col = pal[[i]])
  }
  graphics::legend("topright", legend = as.character(lev), col = pal[seq_along(lev)], lwd = 2, bty = "n")
  invisible(NULL)
}

#' Plot simulated advanced pupil trajectories
#'
#' @param x An advanced pupil simulation.
#' @param observed If TRUE, plot observed condition means; otherwise plot stored
#'   latent means.
#' @param ... Additional graphical arguments.
#' @export
plot_advanced_pupil_simulation <- function(x, observed = TRUE, ...) {
  if (!inherits(x, "gp3bayes_pupil_advanced_simulation")) stop("Expected an advanced pupil simulation.", call. = FALSE)
  d <- x$data
  y <- if (observed) d$pupil else x$truth$mean
  agg <- stats::aggregate(y, list(time_ms = d$time_ms, condition = d$condition), function(z) mean(z, na.rm = TRUE))
  names(agg)[[3L]] <- "mean"
  lev <- levels(d$condition)
  pal <- grDevices::hcl.colors(max(3L, length(lev)), "Dark 3")
  graphics::plot(NA, xlim = range(agg$time_ms), ylim = range(agg$mean, finite = TRUE), xlab = "Time (ms)", ylab = "Pupil", main = if (observed) "Simulated observed trajectories" else "Stored latent mean trajectories", ...)
  for (i in seq_along(lev)) {
    z <- agg[agg$condition == lev[[i]], ]
    graphics::lines(z$time_ms, z$mean, lwd = 2, col = pal[[i]])
  }
  graphics::abline(v = 0, lty = 3)
  graphics::legend("topright", legend = lev, col = pal[seq_along(lev)], lwd = 2, bty = "n")
  invisible(agg)
}

#' Plot an advanced posterior pupil trajectory
#' @param x An advanced trajectory prediction.
#' @param probability Central interval probability.
#' @param ... Additional graphical arguments.
#' @export
plot_advanced_pupil_trajectory <- function(x, probability = 0.95, ...) {
  if (!inherits(x, "gp3bayes_pupil_advanced_trajectory")) stop("Expected an advanced pupil trajectory.", call. = FALSE)
  tab <- advanced_pupil_trajectory_table(x, probability)
  m <- x$specification$mapping
  group <- if (is.null(m$condition)) NULL else tab[[m$condition]]
  .p05_plot_interval(tab[[m$time]], tab$mean, tab$q_low, tab$q_high, group = group, xlab = m$time, ylab = "Posterior pupil", main = "Advanced posterior pupil trajectory", ...)
  invisible(tab)
}

#' Plot posterior residual scale over time
#' @param x A residual-scale estimand.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_residual_scale <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_residual_scale")) stop("Expected a residual-scale estimand.", call. = FALSE)
  tab <- pupil_residual_scale_table(x)
  m <- x$specification$mapping
  group <- if (is.null(m$condition)) NULL else tab[[m$condition]]
  .p05_plot_interval(tab[[m$time]], tab$mean, tab$q_low, tab$q_high, group = group, xlab = m$time, ylab = "Residual sigma", main = paste("Posterior residual scale:", x$residual_scale), ...)
  invisible(tab)
}

#' Plot Gaussian-process hyperparameters
#' @param x A GP-hyperparameter object.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_gp_hyperparameters <- function(x, ...) {
  tab <- pupil_gp_table(x)
  old <- graphics::par(mar = c(8, 4, 3, 1))
  on.exit(graphics::par(old), add = TRUE)
  y <- seq_len(nrow(tab))
  graphics::plot(tab$mean, y, xlim = range(c(tab$q_low, tab$q_high), finite = TRUE), yaxt = "n", ylab = "", xlab = "Posterior value", main = paste0("GP hyperparameters (", x$gp_spec$kernel, ")"), ...)
  graphics::segments(tab$q_low, y, tab$q_high, y)
  graphics::points(tab$median, y, pch = 19)
  graphics::axis(2, at = y, labels = tab$parameter, las = 2, cex.axis = 0.7)
  invisible(tab)
}

#' Plot empirical temporal-dependence audit
#' @param x A temporal-dependence audit.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_temporal_dependence <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_temporal_dependence_audit")) stop("Expected a temporal-dependence audit.", call. = FALSE)
  z <- x$series$lag1
  graphics::hist(z[is.finite(z)], breaks = "FD", xlab = "Within-series lag-1 autocorrelation", main = "Observed temporal dependence", ...)
  graphics::abline(v = stats::median(z, na.rm = TRUE), lwd = 2, lty = 2)
  invisible(x$series)
}

#' Plot residual autocorrelation comparison
#' @param x An autocorrelation comparison object.
#' @param absolute Plot median absolute ACF if TRUE.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_autocorrelation_comparison <- function(x, absolute = TRUE, ...) {
  if (!inherits(x, "gp3bayes_pupil_autocorrelation_comparison")) stop("Expected an autocorrelation comparison.", call. = FALSE)
  tab <- x$table
  models <- unique(tab$model)
  pal <- grDevices::hcl.colors(max(3L, length(models)), "Dark 3")
  colname <- if (absolute) "median_abs_acf" else "median_acf"
  graphics::plot(NA, xlim = range(tab$lag), ylim = range(tab[[colname]], finite = TRUE), xlab = "Lag", ylab = if (absolute) "Median |residual ACF|" else "Median residual ACF", main = "Residual temporal dependence", ...)
  for (i in seq_along(models)) {
    z <- tab[tab$model == models[[i]], ]
    graphics::lines(z$lag, z[[colname]], type = "b", col = pal[[i]], lwd = 2)
  }
  graphics::legend("topright", legend = models, col = pal[seq_along(models)], lwd = 2, bty = "n")
  invisible(tab)
}

#' Plot missing-response fraction over time
#' @param x A missingness audit.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_missingness <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_missingness_audit")) stop("Expected a missingness audit.", call. = FALSE)
  z <- x$by_time
  graphics::barplot(z$response_missing_fraction, names.arg = z$time_bin, las = 2, ylab = "Missing response fraction", main = "Pupil missingness across time", ...)
  invisible(z)
}

#' Plot measurement-uncertainty audit
#' @param x A measurement audit.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_measurement_uncertainty <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_measurement_audit_05")) stop("Expected a measurement audit.", call. = FALSE)
  tab <- x$table
  vals <- rbind(tab$missing_fraction, tab$nonpositive_fraction)
  graphics::barplot(vals, beside = TRUE, names.arg = tab$variable, las = 2, ylab = "Fraction", main = "Measurement-uncertainty audit", legend.text = c("missing SE", "nonpositive/invalid SE"), ...)
  invisible(tab)
}

#' Plot binocular posterior trajectories
#' @param x A binocular trajectory.
#' @param probability Central interval probability.
#' @param ... Additional graphical arguments.
#' @export
plot_binocular_pupil_trajectory <- function(x, probability = x$probability, ...) {
  if (!inherits(x, "gp3bayes_binocular_pupil_trajectory")) stop("Expected a binocular trajectory.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  alpha <- (1 - probability) / 2
  ls <- .p05_quantile_summary(x$left_draws, c(alpha, 0.5, 1 - alpha))
  rs <- .p05_quantile_summary(x$right_draws, c(alpha, 0.5, 1 - alpha))
  time <- x$grid[[x$mapping$time]]
  cond <- x$grid[[x$mapping$condition]]
  groups <- interaction(cond, rep(c("left", "right"), each = length(cond)), drop = TRUE)
  xx <- rep(time, 2)
  mm <- c(ls$mean, rs$mean)
  lo <- c(ls$q_low, rs$q_low)
  hi <- c(ls$q_high, rs$q_high)
  .p05_plot_interval(xx, mm, lo, hi, group = groups, xlab = x$mapping$time, ylab = "Posterior pupil", main = "Joint binocular posterior trajectories", ...)
  invisible(list(left = ls, right = rs))
}

#' Plot predictive model comparison
#' @param x A model-comparison object.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_model_comparison <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_model_comparison")) stop("Expected a pupil model comparison.", call. = FALSE)
  tab <- x$table
  est_col <- if ("elpd_diff" %in% names(tab)) "elpd_diff" else names(tab)[vapply(tab, is.numeric, logical(1L))][[1L]]
  vals <- tab[[est_col]]
  names(vals) <- tab$model
  graphics::dotchart(vals, labels = tab$model, xlab = est_col, main = paste("Predictive comparison:", x$criterion), ...)
  graphics::abline(v = 0, lty = 3)
  invisible(tab)
}

#' Plot leave-future-out scores
#' @param x An executed LFO validation or LFO comparison.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_lfo <- function(x, ...) {
  if (inherits(x, "gp3bayes_pupil_lfo_validation")) {
    if (!x$executed) stop("Execute the LFO validation before plotting scores.", call. = FALSE)
    graphics::plot(x$scores$refit, x$scores$mean_log_score, type = "b", xlab = "Sequential refit", ylab = "Mean future log score", main = "Leave-future-out validation", ...)
    return(invisible(x$scores))
  }
  if (inherits(x, "gp3bayes_pupil_lfo_comparison")) {
    vals <- x$table$total_elpd_future
    names(vals) <- x$table$model
    graphics::dotchart(vals, labels = x$table$model, xlab = "Total future ELPD", main = "Leave-future-out model comparison", ...)
    return(invisible(x$table))
  }
  stop("Expected an LFO validation or comparison.", call. = FALSE)
}

#' Plot experimental nonlinear response parameters
#' @param x A response-parameter object.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_response_parameters <- function(x, ...) {
  tab <- pupil_response_parameter_table(x)
  old <- graphics::par(mar = c(8, 4, 3, 1))
  on.exit(graphics::par(old), add = TRUE)
  y <- seq_len(nrow(tab))
  graphics::plot(tab$mean, y, xlim = range(c(tab$q_low, tab$q_high), finite = TRUE), yaxt = "n", ylab = "", xlab = "Posterior coefficient", main = "Experimental response-shape parameters", ...)
  graphics::segments(tab$q_low, y, tab$q_high, y)
  graphics::points(tab$median, y, pch = 19)
  graphics::axis(2, at = y, labels = tab$parameter, las = 2, cex.axis = 0.7)
  invisible(tab)
}

#' Plot residual spectrum
#' @param x A residual-spectrum object.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_residual_spectrum <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_residual_spectrum")) stop("Expected a residual-spectrum object.", call. = FALSE)
  z <- x$table
  graphics::plot(z$frequency, z$median_power, type = "l", lwd = 2, xlab = "Normalized frequency", ylab = "Median residual power", main = "Residual spectrum", ...)
  graphics::polygon(c(z$frequency, rev(z$frequency)), c(z$q25_power, rev(z$q75_power)), border = NA, col = grDevices::adjustcolor("grey70", alpha.f = 0.4))
  graphics::lines(z$frequency, z$median_power, lwd = 2)
  invisible(z)
}

#' Plot advanced model computational complexity
#' @param x A complexity audit or advanced specification.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_model_complexity <- function(x, ...) {
  if (inherits(x, "gp3bayes_pupil_advanced_specification")) x <- x$complexity_audit
  if (!inherits(x, "gp3bayes_pupil_complexity_audit")) stop("Expected a complexity audit.", call. = FALSE)
  score <- match(x$checks$status, c("ok", "review", "high")) - 1L
  graphics::barplot(score, names.arg = x$checks$check, las = 2, ylim = c(0, 2.2), yaxt = "n", ylab = "Complexity flag", main = paste("Computational complexity:", x$overall_status), ...)
  graphics::axis(2, at = 0:2, labels = c("ok", "review", "high"), las = 1)
  invisible(x$checks)
}

#' Plot posterior pupil trajectory derivative
#' @param x A trajectory-derivative object.
#' @param probability Central posterior interval probability.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_trajectory_derivative <- function(x, probability = x$probability, ...) {
  if (!inherits(x, "gp3bayes_pupil_trajectory_derivative")) stop("Expected a trajectory derivative object.", call. = FALSE)
  tab <- pupil_trajectory_derivative_table(x, probability)
  spec <- x$specification
  ylab <- if (x$order == 1L) "Pupil trajectory derivative" else "Pupil trajectory second derivative"
    group <- if (is.null(spec$mapping$condition)) NULL else tab[[spec$mapping$condition]]
  .p05_plot_interval(
    tab[[spec$mapping$time]], tab$mean, tab$q_low, tab$q_high,
    group = group, xlab = spec$mapping$time, ylab = ylab,
    main = paste0("Posterior temporal derivative (order ", x$order, ")"), ...
  )
}

#' Plot a dynamic pupil condition contrast
#' @param x A dynamic contrast.
#' @param ... Additional base graphics arguments.
#' @export
plot_pupil_dynamic_contrast <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_dynamic_contrast")) stop("Expected a dynamic contrast.", call. = FALSE)
  tab <- x$table
  time_col <- x$specification$mapping$time
  graphics::plot(tab[[time_col]], tab$mean, type = "l", xlab = time_col, ylab = "Posterior contrast", ...)
  graphics::polygon(c(tab[[time_col]], rev(tab[[time_col]])), c(tab$q_low, rev(tab$q_high)), border = NA, density = 18, angle = 45)
  graphics::lines(tab[[time_col]], tab$mean, lwd = 2)
  graphics::abline(h = c(-abs(x$threshold), abs(x$threshold)), lty = 2)
  invisible(x)
}

#' Plot advanced identifiability/design-support audit
#' @param x An identifiability audit.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_identifiability_audit <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_identifiability_audit")) stop("Expected an identifiability audit.", call. = FALSE)
  tab <- x$table
  score <- c(pass = 0, review = 1, high = 2)[tab$status]
  graphics::barplot(score, names.arg = tab$check, las = 2, ylab = "Governance flag (0 pass, 1 review, 2 high)", ...)
  invisible(x)
}

#' Plot posterior predictive calibration
#' @param x A predictive score/calibration object.
#' @param ... Additional graphical arguments.
#' @export
plot_pupil_predictive_calibration <- function(x, ...) {
  if (!inherits(x, "gp3bayes_pupil_predictive_score")) stop("Expected a predictive score object.", call. = FALSE)
  p <- x$pointwise
  graphics::plot(p$predicted_mean, p$observed, xlab = "Posterior predictive mean", ylab = "Observed pupil", ...)
  rr <- range(c(p$predicted_mean, p$observed), finite = TRUE)
  graphics::abline(a = 0, b = 1, lty = 2)
  invisible(x)
}

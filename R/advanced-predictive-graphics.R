# Advanced predictive graphics

utils::globalVariables(c(
  "false_positive_rate", "true_positive_rate", "recall", "precision",
  "predicted_probability", "observed_rate", "calibration_gap",
  "probability", "observed_quantile", "predictive_mean_quantile",
  "predictive_lower_quantile", "predictive_upper_quantile",
  "interval_width", "probability_rank_1", "mean_rank", "draw",
  "statistic_value"
))

#' Prediction-Draw Distribution Plot
#'
#' @param x A `gp3bayes_prediction`.
#' @param observations Optional prediction-row indices.
#' @param max_draws Maximum posterior draws displayed.
#'
#' @return A `ggplot`.
#' @export
plot_prediction_draws <- function(
  x,
  observations = NULL,
  max_draws = 500L
) {
  .gp3g_require("ggplot2", "plot prediction draws")
  d <- prediction_draws_long(x, max_draws = max_draws)

  if (!is.null(observations)) {
    if (!is.numeric(observations) || anyNA(observations)) {
      .gp3p_stop("`observations` must be numeric prediction-row indices.")
    }
    d <- d[d$observation %in% observations, , drop = FALSE]
  }

  if (!nrow(d)) .gp3p_stop("No prediction draws remain for plotting.")

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = factor(observation), y = value)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::labs(
      x = "Prediction row",
      y = "Posterior predicted value",
      title = "Posterior prediction distributions"
    ) +
    theme_gp3bayes()
}

#' Posterior Predictive Statistic Plot
#'
#' @param x A `gp3bayes_ppc_statistic`.
#' @param bins Histogram bins.
#'
#' @return A `ggplot`.
#' @export
plot_ppc_statistic <- function(x, bins = 30L) {
  if (!inherits(x, "gp3bayes_ppc_statistic")) {
    .gp3p_stop("`x` must be a `gp3bayes_ppc_statistic`.")
  }
  .gp3g_require("ggplot2", "plot posterior predictive statistics")
  if (!is.numeric(bins) || length(bins) != 1L || bins < 2 || bins != floor(bins)) {
    .gp3p_stop("`bins` must be one integer greater than or equal to 2.")
  }

  d <- data.frame(statistic_value = x$replicated)
  ggplot2::ggplot(d, ggplot2::aes(x = statistic_value)) +
    ggplot2::geom_histogram(bins = as.integer(bins)) +
    ggplot2::geom_vline(xintercept = x$observed, linetype = 2) +
    ggplot2::labs(
      x = paste("Replicated", x$statistic),
      y = "Posterior predictive draws",
      title = "Posterior predictive discrepancy distribution",
      subtitle = "Dashed line is the observed statistic."
    ) +
    theme_gp3bayes()
}

#' Binary ROC Plot
#'
#' @param x A ROC table, binary expected prediction, or numeric probabilities.
#' @param observed Optional observed outcomes for numeric probabilities.
#'
#' @return A `ggplot`.
#' @export
plot_binary_roc <- function(x, observed = NULL) {
  .gp3g_require("ggplot2", "plot a binary ROC curve")
  d <- if (
    is.data.frame(x) &&
      all(c("false_positive_rate", "true_positive_rate") %in% names(x))
  ) x else binary_roc_curve(x, observed)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = false_positive_rate, y = true_positive_rate)
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_path() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "False-positive rate",
      y = "True-positive rate",
      title = "Binary ROC curve"
    ) +
    theme_gp3bayes()
}

#' Binary Precision-Recall Plot
#'
#' @param x A precision-recall table, binary expected prediction, or numeric
#'   probabilities.
#' @param observed Optional observed outcomes for numeric probabilities.
#'
#' @return A `ggplot`.
#' @export
plot_binary_precision_recall <- function(x, observed = NULL) {
  .gp3g_require("ggplot2", "plot a precision-recall curve")
  d <- if (
    is.data.frame(x) &&
      all(c("recall", "precision") %in% names(x))
  ) x else binary_precision_recall_curve(x, observed)

  ggplot2::ggplot(d, ggplot2::aes(x = recall, y = precision)) +
    ggplot2::geom_path() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Recall",
      y = "Precision",
      title = "Binary precision-recall curve"
    ) +
    theme_gp3bayes()
}

#' Grouped Binary Calibration Plot
#'
#' @param x Grouped calibration table or binary expected prediction.
#' @param group Grouping-column name when `x` is a prediction object.
#'
#' @return A `ggplot`.
#' @export
plot_binary_group_calibration <- function(x, group = NULL) {
  .gp3g_require("ggplot2", "plot grouped binary calibration")
  d <- if (inherits(x, "gp3bayes_prediction")) {
    if (is.null(group)) .gp3p_stop("Supply `group` for a prediction object.")
    binary_group_calibration(x, group)
  } else x

  if (!is.data.frame(d) ||
      !all(c("group", "predicted_probability", "observed_rate") %in% names(d))) {
    .gp3p_stop("`x` does not contain grouped calibration data.")
  }

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = predicted_probability, y = observed_rate, label = group)
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_text(vjust = -0.6, check_overlap = TRUE) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Mean predicted probability",
      y = "Observed rate",
      title = "Grouped binary calibration"
    ) +
    theme_gp3bayes()
}

#' Duration Predictive Q-Q Plot
#'
#' @param x A duration Q-Q table or duration posterior predictive object.
#'
#' @return A `ggplot`.
#' @export
plot_duration_qq <- function(x) {
  .gp3g_require("ggplot2", "plot duration predictive quantiles")
  d <- if (inherits(x, "gp3bayes_prediction")) duration_qq_table(x) else x

  required <- c(
    "observed_quantile", "predictive_mean_quantile",
    "predictive_lower_quantile", "predictive_upper_quantile"
  )
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3p_stop("`x` does not contain duration Q-Q data.")
  }

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = observed_quantile,
      y = predictive_mean_quantile,
      ymin = predictive_lower_quantile,
      ymax = predictive_upper_quantile
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_pointrange() +
    ggplot2::labs(
      x = "Observed quantile",
      y = "Posterior predictive quantile",
      title = "Duration posterior predictive Q-Q check"
    ) +
    theme_gp3bayes()
}

#' Duration Tail-Check Plot
#'
#' @param x A duration tail-check table.
#'
#' @return A `ggplot`.
#' @export
plot_duration_tail <- function(x) {
  .gp3g_require("ggplot2", "plot duration tail checks")
  required <- c(
    "threshold", "observed_tail_rate", "predictive_mean_tail_rate",
    "predictive_lower_tail_rate", "predictive_upper_tail_rate"
  )
  if (!is.data.frame(x) || !all(required %in% names(x))) {
    .gp3p_stop("`x` must be a duration tail-check table.")
  }
  d <- x
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = threshold,
      y = predictive_mean_tail_rate,
      ymin = predictive_lower_tail_rate,
      ymax = predictive_upper_tail_rate
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_point(ggplot2::aes(y = observed_tail_rate), shape = 4) +
    ggplot2::labs(
      x = "Duration threshold",
      y = "Tail rate",
      title = "Duration posterior predictive tail check",
      subtitle = "Cross marks the observed tail rate."
    ) +
    theme_gp3bayes()
}

#' Group Prediction Plot
#'
#' @param x A group prediction summary.
#' @param group_column Name of the grouping column to use on the axis.
#'
#' @return A `ggplot`.
#' @export
plot_group_predictions <- function(x, group_column) {
  .gp3g_require("ggplot2", "plot group prediction summaries")
  if (
    !is.data.frame(x) ||
      !all(c(group_column, "predicted_median", "lower", "upper") %in% names(x))
  ) {
    .gp3p_stop("`x` does not contain the requested group prediction summary.")
  }
  d <- x
  d$.group <- as.character(d[[group_column]])
  d$.group <- factor(d$.group, levels = rev(unique(d$.group)))

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .group,
      y = predicted_median,
      ymin = lower,
      ymax = upper
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = group_column,
      y = "Posterior group prediction",
      title = "Grouped posterior predictions"
    ) +
    theme_gp3bayes()

  if ("observed" %in% names(d) && any(is.finite(d$observed))) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = observed), shape = 4)
  }
  p
}

#' Prediction Interval-Width Plot
#'
#' @param x A prediction object or interval-width table.
#'
#' @return A `ggplot`.
#' @export
plot_prediction_interval_width <- function(x) {
  .gp3g_require("ggplot2", "plot prediction interval widths")
  d <- if (inherits(x, "gp3bayes_prediction")) prediction_interval_width(x) else x
  if (!is.data.frame(d) ||
      !all(c("observation", "interval_width") %in% names(d))) {
    .gp3p_stop("`x` does not contain prediction interval-width data.")
  }

  ggplot2::ggplot(d, ggplot2::aes(x = observation, y = interval_width)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Prediction row",
      y = "Interval width",
      title = "Posterior prediction interval widths"
    ) +
    theme_gp3bayes()
}

#' Prediction Rank-Probability Plot
#'
#' @param x A ranking-probability table.
#'
#' @return A `ggplot`.
#' @export
plot_prediction_rank_probabilities <- function(x) {
  .gp3g_require("ggplot2", "plot prediction ranking probabilities")
  if (!is.data.frame(x) ||
      !all(c("observation", "probability_rank_1") %in% names(x))) {
    .gp3p_stop("`x` must be a prediction ranking-probability table.")
  }

  ggplot2::ggplot(
    x,
    ggplot2::aes(x = factor(observation), y = probability_rank_1)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Prediction row",
      y = "Posterior probability of rank 1",
      title = "Descriptive posterior ranking probabilities",
      subtitle = "The package does not automatically select a row."
    ) +
    theme_gp3bayes()
}

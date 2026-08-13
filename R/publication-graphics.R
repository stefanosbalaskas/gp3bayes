# Publication-oriented ggplot/bayesplot graphics

utils::globalVariables(c(
  "variable", "mean", "lower", "median", "upper", "flagged",
  "metric", "value", "threshold", "observation", "predicted_median",
  "predicted_mean", "observed", "mean_predicted_probability", "observed_rate",
  "posterior_lower", "posterior_upper", "nominal", "empirical",
  "nominal_coverage", "empirical_coverage", "residual", "outside_support",
  "novel_levels", "group", "estimate", "level", "coefficient",
  "expected_response_variance", "total_predictive_variance",
  "residual_component", "pareto_k", "model", "elpd_diff", "se_diff", "weight",
  "quantity", "count", "component", "variance", "pit"
))

.gp3g_require <- function(package, purpose) {
  .gp3p_require(package, purpose)
}

#' gp3bayes Publication Theme
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A `ggplot2` theme.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) theme_gp3bayes()
#' @export
theme_gp3bayes <- function(base_size = 11, base_family = "") {
  .gp3g_require("ggplot2", "construct the gp3bayes plotting theme")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      strip.placement = "outside"
    )
}

#' Posterior Interval Plot
#'
#' @param x A gp3bayes fit or posterior draws accepted by
#'   `posterior_interval_table()`.
#' @param variables,regex Posterior variable selectors.
#' @param prob Inner interval probability.
#' @param prob_outer Outer interval probability.
#'
#' @return A `ggplot` object.
#' @export
plot_posterior_intervals <- function(
  x,
  variables = NULL,
  regex = NULL,
  prob = 0.8,
  prob_outer = 0.95
) {
  .gp3g_require("bayesplot", "plot posterior intervals")
  .gp3g_require("ggplot2", "plot posterior intervals")
  draws <- .gp3p_draw_matrix(x, variables, regex)
  bayesplot::mcmc_intervals(
    draws,
    prob = prob,
    prob_outer = prob_outer
  ) + theme_gp3bayes()
}

#' Posterior Area Plot
#'
#' @inheritParams plot_posterior_intervals
#'
#' @return A `ggplot` object.
#' @export
plot_posterior_areas <- function(
  x,
  variables = NULL,
  regex = NULL,
  prob = 0.5,
  prob_outer = 0.95
) {
  .gp3g_require("bayesplot", "plot posterior areas")
  .gp3g_require("ggplot2", "plot posterior areas")
  draws <- .gp3p_draw_matrix(x, variables, regex)
  bayesplot::mcmc_areas(
    draws,
    prob = prob,
    prob_outer = prob_outer
  ) + theme_gp3bayes()
}

#' Posterior Density Plot
#'
#' @param x A gp3bayes fit or posterior draws.
#' @param variables,regex Posterior variable selectors.
#'
#' @return A `ggplot` object.
#' @export
plot_posterior_density <- function(x, variables = NULL, regex = NULL) {
  .gp3g_require("bayesplot", "plot posterior densities")
  .gp3g_require("ggplot2", "plot posterior densities")
  draws <- .gp3p_draw_matrix(x, variables, regex)
  bayesplot::mcmc_areas(draws, prob = 0.8) + theme_gp3bayes()
}

#' Posterior Pairs Plot
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variables Optional variables.
#' @param regex Optional variable regular expression.
#' @param max_variables Maximum number of variables displayed.
#'
#' @return A bayesplot pairs object.
#' @export
plot_posterior_pairs <- function(
  fit,
  variables = NULL,
  regex = "^b_",
  max_variables = 8L
) {
  .gp3p_validate_fit(fit)
  .gp3g_require("bayesplot", "plot posterior pairs")
  draws <- extract_posterior_draws(
    fit,
    variables = variables,
    regex = regex,
    format = "array"
  )
  vars <- posterior::variables(draws)
  if (length(vars) > max_variables) {
    vars <- vars[seq_len(max_variables)]
    draws <- posterior::subset_draws(draws, variable = vars)
  }
  bayesplot::mcmc_pairs(draws)
}

#' Rank-Diagnostic Plot
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variables Optional posterior variables.
#' @param regex Optional posterior-variable regular expression.
#'
#' @return A `ggplot` rank-overlay diagnostic.
#' @export
plot_rank_diagnostics <- function(
  fit,
  variables = NULL,
  regex = "^b_"
) {
  .gp3p_validate_fit(fit)
  .gp3g_require("bayesplot", "plot rank diagnostics")
  .gp3g_require("ggplot2", "plot rank diagnostics")
  draws <- extract_posterior_draws(
    fit,
    variables = variables,
    regex = regex,
    format = "array"
  )
  bayesplot::mcmc_rank_overlay(draws) + theme_gp3bayes()
}

#' Autocorrelation Diagnostic Plot
#'
#' @inheritParams plot_rank_diagnostics
#' @param lags Maximum autocorrelation lag.
#'
#' @return A `ggplot` object.
#' @export
plot_autocorrelation <- function(
  fit,
  variables = NULL,
  regex = "^b_",
  lags = 20L
) {
  .gp3p_validate_fit(fit)
  .gp3g_require("bayesplot", "plot MCMC autocorrelation")
  .gp3g_require("ggplot2", "plot MCMC autocorrelation")
  draws <- extract_posterior_draws(
    fit,
    variables = variables,
    regex = regex,
    format = "array"
  )
  bayesplot::mcmc_acf_bar(draws, lags = lags) + theme_gp3bayes()
}

#' MCMC Review-Flag Plot
#'
#' @param x A fitted gp3bayes object or `gp3bayes_mcmc_quality`.
#'
#' @return A `ggplot` showing parameter-level review flags.
#' @export
plot_mcmc_quality <- function(x) {
  .gp3g_require("ggplot2", "plot MCMC quality flags")
  quality <- if (inherits(x, "gp3bayes_mcmc_quality")) x else summarise_mcmc_quality(x)
  d <- quality$issues
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = as.integer(flagged))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = c(0, 1), labels = c("No flag", "Review")) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "MCMC parameter review flags",
      subtitle = "Flags request inspection; absence of a flag is not an adequacy claim."
    ) +
    theme_gp3bayes()
}

#' Sampler-Diagnostic Plot
#'
#' @param fit A fitted `gp3bayes_fit`.
#'
#' @return A `ggplot` of sampler metrics relative to review thresholds.
#' @export
plot_sampler_diagnostics <- function(fit) {
  .gp3g_require("ggplot2", "plot sampler diagnostics")
  d <- sampler_diagnostic_table(fit)
  if (!nrow(d)) .gp3p_stop("No sampler diagnostics are available.")
  d$metric <- factor(d$metric, levels = rev(d$metric))
  ggplot2::ggplot(d, ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Observed diagnostic",
      title = "NUTS sampler diagnostics"
    ) +
    theme_gp3bayes()
}

#' Posterior Correlation Plot
#'
#' @param x A gp3bayes fit or posterior draws.
#' @param variables,regex Posterior variable selectors.
#' @param method Correlation method.
#'
#' @return A `ggplot` heatmap of posterior-draw correlations.
#' @export
plot_posterior_correlations <- function(
  x,
  variables = NULL,
  regex = NULL,
  method = c("pearson", "spearman")
) {
  .gp3g_require("ggplot2", "plot posterior correlations")
  method <- match.arg(method)
  d <- posterior_correlation_table(
    x,
    variables = variables,
    regex = regex,
    method = method
  )
  reverse <- data.frame(
    variable_1 = d$variable_2,
    variable_2 = d$variable_1,
    correlation = d$correlation,
    method = d$method,
    stringsAsFactors = FALSE
  )
  diag_vars <- unique(c(d$variable_1, d$variable_2))
  diagonal <- data.frame(
    variable_1 = diag_vars,
    variable_2 = diag_vars,
    correlation = 1,
    method = method,
    stringsAsFactors = FALSE
  )
  full <- rbind(d, reverse, diagonal)
  ggplot2::ggplot(
    full,
    ggplot2::aes(x = variable_1, y = variable_2, fill = correlation)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Correlation",
      title = "Posterior-draw correlation matrix"
    ) +
    theme_gp3bayes() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Estimand Interval Plot
#'
#' @param x A `gp3bayes_estimand`.
#' @param quantities Optional estimand quantities.
#' @param probs Posterior interval probabilities.
#'
#' @return A `ggplot` object.
#' @export
plot_estimand_intervals <- function(
  x,
  quantities = NULL,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!inherits(x, "gp3bayes_estimand")) {
    .gp3p_stop("`x` must be a gp3bayes estimand.")
  }
  .gp3g_require("ggplot2", "plot posterior estimands")
  d <- summarise_estimand_draws(x, quantities = quantities, probs = probs)
  d$quantity <- factor(d$quantity, levels = rev(d$quantity))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = quantity, y = median, ymin = lower, ymax = upper)
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Posterior estimand",
      title = "Posterior estimand intervals"
    ) +
    theme_gp3bayes()
}

#' Prediction-Interval Plot
#'
#' @param x A `gp3bayes_prediction`.
#' @param max_rows Maximum rows displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_prediction_intervals <- function(x, max_rows = 100L) {
  if (!inherits(x, "gp3bayes_prediction")) {
    .gp3p_stop("`x` must be a gp3bayes prediction.")
  }
  .gp3g_require("ggplot2", "plot prediction intervals")
  d <- x$summary
  if (nrow(d) > max_rows) d <- d[seq_len(max_rows), , drop = FALSE]
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = observation,
      y = predicted_median,
      ymin = lower,
      ymax = upper
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::labs(
      x = "Prediction row",
      y = "Posterior prediction",
      title = paste("Posterior", x$type, "prediction intervals")
    ) +
    theme_gp3bayes()
  if ("observed" %in% names(d)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = observation, y = observed),
      shape = 4
    )
  }
  p
}

#' @export
plot.gp3bayes_prediction <- function(x, ...) plot_prediction_intervals(x, ...)

#' Binary Calibration Plot
#'
#' @param x A binary calibration table or binary expected prediction.
#' @param bins Number of bins if `x` is a prediction object.
#'
#' @return A `ggplot` object.
#' @export
plot_binary_calibration <- function(x, bins = 10L) {
  .gp3g_require("ggplot2", "plot binary calibration")
  d <- if (inherits(x, "gp3bayes_prediction")) {
    binary_calibration_table(x, bins = bins)
  } else {
    x
  }
  required <- c(
    "mean_predicted_probability", "observed_rate",
    "posterior_lower", "posterior_upper"
  )
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3p_stop("`x` does not contain a binary calibration table.")
  }
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = mean_predicted_probability,
      y = observed_rate,
      ymin = posterior_lower,
      ymax = posterior_upper
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Mean predicted probability",
      y = "Observed event rate",
      title = "Binary calibration"
    ) +
    theme_gp3bayes()
}

#' Binary Threshold-Metric Plot
#'
#' @param x Threshold-metric table, binary expected prediction, or numeric
#'   probabilities.
#' @param observed Optional observed outcomes for numeric predictions.
#' @param thresholds Thresholds to evaluate when `x` is not already a table.
#'
#' @return A `ggplot` object.
#' @export
plot_binary_threshold_metrics <- function(
  x,
  observed = NULL,
  thresholds = seq(0.1, 0.9, by = 0.05)
) {
  .gp3g_require("ggplot2", "plot threshold metrics")
  d <- if (is.data.frame(x) && all(c("threshold", "balanced_accuracy") %in% names(x))) {
    x
  } else {
    binary_threshold_metrics(x, observed, thresholds)
  }
  long <- rbind(
    data.frame(threshold = d$threshold, metric = "accuracy", value = d$accuracy),
    data.frame(threshold = d$threshold, metric = "sensitivity", value = d$sensitivity),
    data.frame(threshold = d$threshold, metric = "specificity", value = d$specificity),
    data.frame(
      threshold = d$threshold,
      metric = "balanced_accuracy",
      value = d$balanced_accuracy
    )
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = threshold, y = value, linetype = metric)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Classification threshold",
      y = "Metric",
      linetype = NULL,
      title = "Threshold-dependent binary prediction metrics"
    ) +
    theme_gp3bayes()
}

#' Duration Quantile-Calibration Plot
#'
#' @param x A duration quantile-calibration table or duration predictive object.
#'
#' @return A `ggplot` object.
#' @export
plot_duration_quantile_calibration <- function(x) {
  .gp3g_require("ggplot2", "plot duration quantile calibration")
  d <- if (inherits(x, "gp3bayes_prediction")) {
    duration_quantile_calibration(x)
  } else x
  if (!is.data.frame(d) || !all(c("nominal", "empirical") %in% names(d))) {
    .gp3p_stop("`x` does not contain duration quantile-calibration data.")
  }
  ggplot2::ggplot(d, ggplot2::aes(x = nominal, y = empirical)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Nominal predictive quantile",
      y = "Empirical fraction below predictive quantile",
      title = "Duration quantile calibration"
    ) +
    theme_gp3bayes()
}

#' Duration PIT Plot
#'
#' @param x A duration PIT table or duration posterior predictive object.
#' @param bins Number of histogram bins.
#'
#' @return A `ggplot` object.
#' @export
plot_duration_pit <- function(x, bins = 10L) {
  .gp3g_require("ggplot2", "plot duration PIT values")
  d <- if (inherits(x, "gp3bayes_prediction")) duration_pit_table(x) else x
  if (!is.data.frame(d) || !"pit" %in% names(d)) {
    .gp3p_stop("`x` does not contain duration PIT values.")
  }
  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) ||
      !is.finite(bins) || bins < 2 || bins != floor(bins)) {
    .gp3p_stop("`bins` must be one integer >= 2.")
  }
  ggplot2::ggplot(d, ggplot2::aes(x = pit)) +
    ggplot2::geom_histogram(
      bins = as.integer(bins),
      boundary = 0,
      closed = "left"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(
      x = "Posterior predictive PIT",
      y = "Observations",
      title = "Duration posterior predictive PIT"
    ) +
    theme_gp3bayes()
}

#' Prediction Exceedance-Probability Plot
#'
#' @param x A table returned by `prediction_exceedance_probability()`.
#'
#' @return A `ggplot` object.
#' @export
plot_exceedance_probability <- function(x) {
  .gp3g_require("ggplot2", "plot posterior exceedance probabilities")
  if (!is.data.frame(x) ||
      !all(c("observation", "probability", "threshold", "direction") %in% names(x))) {
    .gp3p_stop("`x` must be an exceedance-probability table.")
  }
  ggplot2::ggplot(x, ggplot2::aes(x = observation, y = probability)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Prediction row",
      y = "Posterior probability",
      title = paste0(
        "Probability ",
        unique(x$direction)[[1L]],
        " threshold ",
        unique(x$threshold)[[1L]]
      )
    ) +
    theme_gp3bayes()
}

#' Predictive-Coverage Plot
#'
#' @param x Predictive coverage table or posterior predictive object.
#'
#' @return A `ggplot` object.
#' @export
plot_predictive_coverage <- function(x) {
  .gp3g_require("ggplot2", "plot predictive coverage")
  d <- if (inherits(x, "gp3bayes_prediction")) {
    predictive_coverage_table(x)
  } else x
  if (!is.data.frame(d) ||
      !all(c("nominal_coverage", "empirical_coverage") %in% names(d))) {
    .gp3p_stop("`x` does not contain predictive-coverage data.")
  }
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = nominal_coverage, y = empirical_coverage)
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Nominal coverage",
      y = "Empirical coverage",
      title = "Posterior predictive interval coverage"
    ) +
    theme_gp3bayes()
}

#' Predictive-Residual Plot
#'
#' @param x Residual table returned by `predictive_residuals()`.
#'
#' @return A `ggplot` object.
#' @export
plot_predictive_residuals <- function(x) {
  .gp3g_require("ggplot2", "plot predictive residuals")
  if (!is.data.frame(x) || !all(c("expected", "residual") %in% names(x))) {
    .gp3p_stop("`x` must be a predictive residual table.")
  }
  ggplot2::ggplot(x, ggplot2::aes(x = expected, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Posterior expected response",
      y = "Residual",
      title = "Posterior predictive residual review"
    ) +
    theme_gp3bayes()
}

#' Prediction-Support Plot
#'
#' @param x A prediction-support audit or its table.
#'
#' @return A `ggplot` object.
#' @export
plot_prediction_support <- function(x) {
  .gp3g_require("ggplot2", "plot prediction support")
  d <- if (inherits(x, "gp3bayes_prediction_support")) x$table else x
  if (!is.data.frame(d) || !all(c("variable", "outside_support", "novel_levels") %in% names(d))) {
    .gp3p_stop("`x` does not contain prediction-support diagnostics.")
  }
  d$count <- d$outside_support + ifelse(is.na(d$novel_levels), 0, d$novel_levels)
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = count)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Flag count",
      title = "Prediction-support review"
    ) +
    theme_gp3bayes()
}

#' Prediction-Uncertainty Plot
#'
#' @param x A `gp3bayes_prediction_uncertainty`.
#' @param max_rows Maximum observations displayed.
#'
#' @return A `ggplot` object.
#' @export
plot_uncertainty_decomposition <- function(x, max_rows = 100L) {
  if (!inherits(x, "gp3bayes_prediction_uncertainty")) {
    .gp3p_stop("`x` must be a gp3bayes prediction-uncertainty object.")
  }
  .gp3g_require("ggplot2", "plot prediction uncertainty")
  d <- x$table
  if (nrow(d) > max_rows) d <- d[seq_len(max_rows), , drop = FALSE]
  long <- rbind(
    data.frame(
      observation = d$observation,
      component = "expected-response",
      variance = d$expected_response_variance
    ),
    data.frame(
      observation = d$observation,
      component = "residual",
      variance = d$residual_component
    )
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = observation, y = variance, fill = component)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Prediction row",
      y = "Posterior variance",
      fill = NULL,
      title = "Descriptive prediction-uncertainty decomposition"
    ) +
    theme_gp3bayes()
}

#' @export
plot.gp3bayes_prediction_uncertainty <- function(x, ...) {
  plot_uncertainty_decomposition(x, ...)
}

#' Grouped Posterior Predictive Plot
#'
#' @param x A `gp3bayes_group_prediction_check`.
#'
#' @return A `ggplot` object.
#' @export
plot_grouped_prediction_check <- function(x) {
  if (!inherits(x, "gp3bayes_group_prediction_check")) {
    .gp3p_stop("`x` must be a grouped prediction check.")
  }
  .gp3g_require("ggplot2", "plot grouped posterior predictions")
  d <- x$table
  d$group <- factor(d$group, levels = rev(d$group))
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = group,
      y = predicted_median,
      ymin = lower,
      ymax = upper
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_point(ggplot2::aes(y = observed), shape = 4) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Group outcome summary",
      title = "Grouped posterior predictive review"
    ) +
    theme_gp3bayes()
}

#' @export
plot.gp3bayes_group_prediction_check <- function(x, ...) {
  plot_grouped_prediction_check(x)
}

#' Group-Effect Plot
#'
#' @param x A group-effect table or fitted gp3bayes object.
#' @param groups Optional grouping factors when `x` is a fit.
#'
#' @return A faceted `ggplot` object.
#' @export
plot_group_effects <- function(x, groups = NULL) {
  .gp3g_require("ggplot2", "plot group-level effects")
  d <- if (inherits(x, "gp3bayes_fit")) group_effect_table(x, groups = groups) else x
  if (!is.data.frame(d) ||
      !all(c("group", "level", "coefficient", "estimate", "lower", "upper") %in% names(d))) {
    .gp3p_stop("`x` does not contain group-effect summaries.")
  }
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = level,
      y = estimate,
      ymin = lower,
      ymax = upper
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(coefficient ~ group, scales = "free_y", space = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Group-level deviation",
      title = "Posterior group-level effects"
    ) +
    theme_gp3bayes()
}

#' Variance-Component Plot
#'
#' @param x A variance-component table or fitted gp3bayes object.
#'
#' @return A `ggplot` object.
#' @export
plot_variance_components <- function(x) {
  .gp3g_require("ggplot2", "plot variance components")
  d <- if (inherits(x, "gp3bayes_fit")) variance_component_table(x) else x
  if (!is.data.frame(d) ||
      !all(c("variable", "median", "lower", "upper") %in% names(d))) {
    .gp3p_stop("`x` does not contain variance-component summaries.")
  }
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = variable, y = median, ymin = lower, ymax = upper)
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Posterior component",
      title = "Variance and correlation components"
    ) +
    theme_gp3bayes()
}

#' LOO Influence Plot
#'
#' @param x A gp3bayes PSIS-LOO object, `loo` object, or LOO diagnostic table.
#'
#' @return A `ggplot` object.
#' @export
plot_loo_influence <- function(x) {
  .gp3g_require("ggplot2", "plot LOO influence diagnostics")
  d <- if (is.data.frame(x) && all(c("observation", "pareto_k") %in% names(x))) {
    x
  } else {
    loo_diagnostic_table(x)
  }
  ggplot2::ggplot(d, ggplot2::aes(x = observation, y = pareto_k)) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.7, 1), linetype = c(3, 2, 1)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Observation",
      y = "Pareto k",
      title = "PSIS-LOO influence diagnostics"
    ) +
    theme_gp3bayes()
}

#' LOO Model-Comparison Plot
#'
#' @param x A gp3bayes LOO comparison or comparison table.
#'
#' @return A `ggplot` object.
#' @export
plot_model_comparison <- function(x) {
  .gp3g_require("ggplot2", "plot LOO model comparisons")
  d <- if (is.data.frame(x) && all(c("model", "elpd_diff", "se_diff") %in% names(x))) {
    x
  } else {
    model_comparison_table(x)
  }
  d$model <- factor(d$model, levels = rev(d$model))
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = model,
      y = elpd_diff,
      ymin = elpd_diff - se_diff,
      ymax = elpd_diff + se_diff
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "ELPD difference (+/- 1 SE)",
      title = "Descriptive PSIS-LOO model comparison"
    ) +
    theme_gp3bayes()
}

#' LOO Model-Weight Plot
#'
#' @param x A gp3bayes LOO weight object or weight table.
#'
#' @return A `ggplot` object.
#' @export
plot_model_weights <- function(x) {
  .gp3g_require("ggplot2", "plot LOO model weights")
  d <- if (is.data.frame(x) && all(c("model", "weight") %in% names(x))) {
    x
  } else {
    model_weights_table(x)
  }
  d$model <- factor(d$model, levels = rev(d$model))
  ggplot2::ggplot(d, ggplot2::aes(x = model, y = weight)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Predictive weight",
      title = "LOO predictive model weights",
      subtitle = "Weights do not automatically select a substantively correct model."
    ) +
    theme_gp3bayes()
}

#' Create a Named Figure Set
#'
#' @param ... Named plot objects.
#' @param title Optional figure-set title.
#'
#' @return A `gp3bayes_figure_set`.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
#'     ggplot2::geom_point()
#'   create_figure_set(example = p)
#' }
#' @export
create_figure_set <- function(..., title = "gp3bayes figure set") {
  plots <- list(...)
  if (!length(plots)) .gp3p_stop("At least one plot is required.")
  if (is.null(names(plots)) || any(!nzchar(names(plots))) || anyDuplicated(names(plots))) {
    .gp3p_stop("All plots must have unique non-empty names.")
  }
  valid <- vapply(
    plots,
    function(p) inherits(p, "ggplot") || inherits(p, "bayesplot_grid") ||
      inherits(p, "gtable"),
    logical(1L)
  )
  if (!all(valid)) {
    .gp3p_stop("All supplied objects must be plot objects.")
  }
  structure(
    list(
      title = title,
      plots = plots,
      names = names(plots)
    ),
    class = "gp3bayes_figure_set"
  )
}

#' @export
print.gp3bayes_figure_set <- function(x, ...) {
  cat("\n", x$title, "\n", sep = "")
  cat(" Figures: ", length(x$plots), "\n", sep = "")
  cat(" ", paste(x$names, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Save a Figure Set
#'
#' File output is explicit: no current-directory default is provided.
#'
#' @param x A `gp3bayes_figure_set`.
#' @param directory Existing or creatable output directory.
#' @param width,height Figure dimensions in inches.
#' @param dpi Raster resolution.
#' @param device File extension/device, such as `"png"` or `"pdf"`.
#' @param overwrite Whether existing files may be replaced.
#'
#' @return Invisibly, the written file paths.
#' @export
save_figure_set <- function(
  x,
  directory,
  width = 7,
  height = 5,
  dpi = 300,
  device = "png",
  overwrite = FALSE
) {
  if (!inherits(x, "gp3bayes_figure_set")) {
    .gp3p_stop("`x` must be a gp3bayes figure set.")
  }
  .gp3g_require("ggplot2", "save publication figures")
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) ||
      !nzchar(directory)) {
    .gp3p_stop("`directory` must be one explicit path.")
  }
  overwrite <- .gp3pr_flag(overwrite, "overwrite")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) .gp3p_stop("Could not create `directory`.")

  safe_names <- gsub("[^A-Za-z0-9._-]+", "-", x$names)
  files <- file.path(directory, paste0(safe_names, ".", device))
  if (!overwrite && any(file.exists(files))) {
    .gp3p_stop("One or more target figure files already exist.")
  }

  for (i in seq_along(files)) {
    ggplot2::ggsave(
      filename = files[[i]],
      plot = x$plots[[i]],
      width = width,
      height = height,
      dpi = dpi,
      device = device
    )
  }
  invisible(files)
}

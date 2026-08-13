# Recovery, sensitivity, and SBC publication adapters

.gp3rs_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3rs_class <- function(x, class_name, label = "x") {
  if (!inherits(x, class_name)) {
    .gp3rs_stop("`", label, "` must inherit from `", class_name, "`.")
  }
  invisible(x)
}

.gp3rs_df <- function(x, field, class_name) {
  .gp3rs_class(x, class_name)
  out <- x[[field]]
  if (!is.data.frame(out)) {
    .gp3rs_stop("`x$", field, "` is not a data frame.")
  }
  out
}

#' Recovery Parameter Summary Table
#'
#' @param x A `gp3bayes_recovery`.
#' @return Parameter-level recovery summaries.
#' @export
recovery_parameter_table <- function(x) {
  .gp3rs_df(x, "parameter_summary", "gp3bayes_recovery")
}

#' Recovery Estimate Table
#'
#' @param x A `gp3bayes_recovery`.
#' @return Repetition-level truth, posterior summaries, and coverage records.
#' @export
recovery_estimate_table <- function(x) {
  .gp3rs_df(x, "estimates", "gp3bayes_recovery")
}

#' Recovery Fit-Status Table
#'
#' @param x A `gp3bayes_recovery`.
#' @return Repetition-level completion and diagnostic-status records.
#' @export
recovery_fit_status_table <- function(x) {
  .gp3rs_df(x, "fit_status", "gp3bayes_recovery")
}

#' Prior Sensitivity Table
#'
#' @param x A `gp3bayes_prior_sensitivity`.
#' @return Parameter-by-scenario posterior shifts.
#' @export
prior_sensitivity_table <- function(x) {
  .gp3rs_df(x, "comparison", "gp3bayes_prior_sensitivity")
}

#' Prior Sensitivity Scenario Table
#'
#' @param x A `gp3bayes_prior_sensitivity`.
#' @return Scenario-level maximum shifts and diagnostic statuses.
#' @export
prior_sensitivity_scenario_table <- function(x) {
  .gp3rs_df(x, "scenario_status", "gp3bayes_prior_sensitivity")
}

#' Estimand Sensitivity Table
#'
#' @param x A `gp3bayes_estimand_sensitivity`.
#' @return Alternative-versus-reference estimand summaries.
#' @export
estimand_sensitivity_table <- function(x) {
  .gp3rs_df(x, "table", "gp3bayes_estimand_sensitivity")
}

#' Group-Deletion Sensitivity Table
#'
#' @param x A `gp3bayes_group_deletion_sensitivity`.
#' @return Omitted-unit estimand summaries.
#' @export
group_deletion_sensitivity_table <- function(x) {
  .gp3rs_df(x, "summary", "gp3bayes_group_deletion_sensitivity")
}

#' Random-Slope Sensitivity Table
#'
#' @param x A `gp3bayes_random_slope_sensitivity`.
#' @return The retained estimand-sensitivity comparison.
#' @export
random_slope_sensitivity_table <- function(x) {
  .gp3rs_class(x, "gp3bayes_random_slope_sensitivity")
  if (!inherits(x$comparison, "gp3bayes_estimand_sensitivity")) {
    .gp3rs_stop("The random-slope object has no valid estimand comparison.")
  }
  estimand_sensitivity_table(x$comparison)
}

#' Power-Scale Sensitivity Table
#'
#' @param x A `gp3bayes_powerscale_sensitivity`.
#' @return The tabular representation provided by `priorsense`.
#' @export
powerscale_sensitivity_table <- function(x) {
  .gp3rs_class(x, "gp3bayes_powerscale_sensitivity")
  tryCatch(
    as.data.frame(x$raw),
    error = function(e) {
      .gp3rs_stop(
        "Could not convert the underlying priorsense result: ",
        conditionMessage(e)
      )
    }
  )
}

#' SBC Statistics Table
#'
#' @param x A `gp3bayes_sbc_result`.
#' @return The statistics table retained by the SBC result.
#' @export
sbc_stats_table <- function(x) {
  .gp3rs_class(x, "gp3bayes_sbc_result")
  out <- tryCatch(as.data.frame(x$raw$stats), error = function(e) NULL)
  if (!is.data.frame(out)) {
    .gp3rs_stop("The SBC result does not expose tabular statistics.")
  }
  out
}

.gp3rs_one_character <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  as.character(x)[[1L]]
}

.gp3rs_one_integer <- function(x) {
  if (is.null(x) || !length(x)) return(NA_integer_)
  as.integer(x[[1L]])
}

#' SBC Overview Table
#'
#' @param x A `gp3bayes_sbc_result`.
#' @return A conservative one-row metadata summary.
#' @export
sbc_overview_table <- function(x) {
  .gp3rs_class(x, "gp3bayes_sbc_result")
  stats <- sbc_stats_table(x)
  variable_col <- intersect(c("variable", "parameter"), names(stats))
  n_variables <- if (length(variable_col)) {
    length(unique(as.character(stats[[variable_col[[1L]]]])))
  } else {
    NA_integer_
  }

  data.frame(
    status = .gp3rs_one_character(x$status),
    family = .gp3rs_one_character(x$plan$family),
    backend = .gp3rs_one_character(x$plan$backend),
    simulations = .gp3rs_one_integer(x$plan$n_sims),
    variables_recorded = n_variables,
    diagnostics_inspected = if (
      is.null(x$diagnostics_inspected) || !length(x$diagnostics_inspected)
    ) NA else as.logical(x$diagnostics_inspected)[[1L]],
    calibration_established = FALSE,
    stringsAsFactors = FALSE
  )
}

.gp3rs_plot_table <- function(x, class_name, extractor) {
  if (inherits(x, class_name)) extractor(x) else x
}

#' Plot Recovery Standardized Bias
#'
#' @param x A recovery object or recovery parameter table.
#' @return A `ggplot`.
#' @export
plot_recovery_bias <- function(x) {
  .gp3g_require("ggplot2", "plot recovery bias")
  d <- .gp3rs_plot_table(x, "gp3bayes_recovery", recovery_parameter_table)
  if (!is.data.frame(d) ||
      !all(c("variable", "standardized_bias") %in% names(d))) {
    .gp3rs_stop("`x` does not contain recovery standardized bias.")
  }
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = standardized_bias)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Standardized bias",
      title = "Parameter-recovery bias"
    ) +
    theme_gp3bayes()
}

#' Plot Recovery Coverage
#'
#' @param x A recovery object or recovery parameter table.
#' @return A `ggplot`.
#' @export
plot_recovery_coverage <- function(x) {
  .gp3g_require("ggplot2", "plot recovery coverage")
  d <- .gp3rs_plot_table(x, "gp3bayes_recovery", recovery_parameter_table)
  if (!is.data.frame(d) || !all(c("variable", "coverage") %in% names(d))) {
    .gp3rs_stop("`x` does not contain recovery coverage.")
  }
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = coverage)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = NULL,
      y = "Empirical interval coverage",
      title = "Parameter-recovery coverage"
    ) +
    theme_gp3bayes()
}

#' Plot Recovery RMSE
#'
#' @param x A recovery object or recovery parameter table.
#' @return A `ggplot`.
#' @export
plot_recovery_rmse <- function(x) {
  .gp3g_require("ggplot2", "plot recovery RMSE")
  d <- .gp3rs_plot_table(x, "gp3bayes_recovery", recovery_parameter_table)
  if (!is.data.frame(d) || !all(c("variable", "rmse") %in% names(d))) {
    .gp3rs_stop("`x` does not contain recovery RMSE.")
  }
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = rmse)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "RMSE", title = "Parameter-recovery RMSE") +
    theme_gp3bayes()
}

#' Plot Repetition-Level Recovery Estimates
#'
#' @param x A recovery object or repetition-level estimate table.
#' @param variables Optional variables to retain.
#' @return A faceted `ggplot`.
#' @export
plot_recovery_estimates <- function(x, variables = NULL) {
  .gp3g_require("ggplot2", "plot repetition-level recovery")
  d <- .gp3rs_plot_table(x, "gp3bayes_recovery", recovery_estimate_table)
  required <- c("variable", "truth", "median", "lower", "upper", "repetition")
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3rs_stop("`x` does not contain repetition-level recovery estimates.")
  }
  if (!is.null(variables)) d <- d[d$variable %in% variables, , drop = FALSE]
  if (!nrow(d)) .gp3rs_stop("No recovery rows remain after filtering.")

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = repetition,
      y = median,
      ymin = lower,
      ymax = upper
    )
  ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = truth), linetype = 2) +
    ggplot2::geom_pointrange() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(
      x = "Recovery repetition",
      y = "Posterior estimate",
      title = "Repetition-level parameter recovery"
    ) +
    theme_gp3bayes()
}

#' Plot Recovery Fit Status
#'
#' @param x A recovery object or fit-status table.
#' @return A `ggplot`.
#' @export
plot_recovery_fit_status <- function(x) {
  .gp3g_require("ggplot2", "plot recovery fit status")
  d <- .gp3rs_plot_table(x, "gp3bayes_recovery", recovery_fit_status_table)
  if (!is.data.frame(d) ||
      !all(c("diagnostic_status", "completed") %in% names(d))) {
    .gp3rs_stop("`x` does not contain recovery fit statuses.")
  }
  tab <- as.data.frame(
    table(
      diagnostic_status = as.character(d$diagnostic_status),
      completed = as.character(d$completed)
    ),
    stringsAsFactors = FALSE
  )
  names(tab)[names(tab) == "Freq"] <- "count"

  ggplot2::ggplot(
    tab,
    ggplot2::aes(x = diagnostic_status, y = count, fill = completed)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      x = "Diagnostic status",
      y = "Repetitions",
      fill = "Completed",
      title = "Recovery-fit completion and diagnostics"
    ) +
    theme_gp3bayes()
}

#' Plot Prior-Scale Sensitivity
#'
#' @param x A prior-sensitivity object or comparison table.
#' @return A faceted `ggplot`.
#' @export
plot_prior_sensitivity <- function(x) {
  .gp3g_require("ggplot2", "plot prior-scale sensitivity")
  d <- .gp3rs_plot_table(
    x,
    "gp3bayes_prior_sensitivity",
    prior_sensitivity_table
  )
  required <- c("scenario", "scale_multiplier", "variable", "standardized_shift")
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3rs_stop("`x` does not contain a prior-sensitivity comparison table.")
  }
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = scale_multiplier,
      y = standardized_shift,
      group = scenario,
      linetype = scenario
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(
      x = "Prior scale multiplier",
      y = "Absolute standardized posterior-median shift",
      linetype = "Scenario",
      title = "Declared prior-scale sensitivity"
    ) +
    theme_gp3bayes()
}

#' Plot Prior-Sensitivity Scenario Maxima
#'
#' @param x A prior-sensitivity object or scenario table.
#' @return A `ggplot`.
#' @export
plot_prior_sensitivity_scenarios <- function(x) {
  .gp3g_require("ggplot2", "plot prior-sensitivity scenarios")
  d <- .gp3rs_plot_table(
    x,
    "gp3bayes_prior_sensitivity",
    prior_sensitivity_scenario_table
  )
  if (!is.data.frame(d) ||
      !all(c("scenario", "maximum_standardized_shift") %in% names(d))) {
    .gp3rs_stop("`x` does not contain scenario-level prior sensitivity.")
  }
  d$scenario <- factor(d$scenario, levels = rev(d$scenario))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = scenario, y = maximum_standardized_shift)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Maximum standardized shift",
      title = "Prior-sensitivity scenario maxima"
    ) +
    theme_gp3bayes()
}

#' Plot Estimand Sensitivity with ggplot2
#'
#' @param x An estimand-sensitivity object or its table.
#' @return A `ggplot`.
#' @export
plot_estimand_sensitivity_gg <- function(x) {
  .gp3g_require("ggplot2", "plot estimand sensitivity")
  d <- .gp3rs_plot_table(
    x,
    "gp3bayes_estimand_sensitivity",
    estimand_sensitivity_table
  )
  required <- c(
    "alternative", "reference_median", "alternative_median",
    "alternative_lower", "alternative_upper"
  )
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3rs_stop("`x` does not contain estimand-sensitivity summaries.")
  }
  d$alternative <- factor(d$alternative, levels = rev(d$alternative))
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = alternative,
      y = alternative_median,
      ymin = alternative_lower,
      ymax = alternative_upper
    )
  ) +
    ggplot2::geom_hline(
      yintercept = unique(d$reference_median)[[1L]],
      linetype = 2
    ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Posterior estimand",
      title = "Estimand sensitivity",
      subtitle = "Dashed line is the reference posterior median."
    ) +
    theme_gp3bayes()
}

#' Plot Group-Deletion Sensitivity
#'
#' @param x A group-deletion object or summary table.
#' @return A `ggplot`.
#' @export
plot_group_deletion_sensitivity <- function(x) {
  .gp3g_require("ggplot2", "plot group-deletion sensitivity")
  d <- .gp3rs_plot_table(
    x,
    "gp3bayes_group_deletion_sensitivity",
    group_deletion_sensitivity_table
  )
  if (!is.data.frame(d) ||
      !all(c("omitted_unit", "median_shift", "status") %in% names(d))) {
    .gp3rs_stop("`x` does not contain group-deletion summaries.")
  }
  d$omitted_unit <- factor(d$omitted_unit, levels = rev(d$omitted_unit))
  ggplot2::ggplot(d, ggplot2::aes(x = omitted_unit, y = median_shift)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Omitted unit",
      y = "Posterior median shift",
      title = "Declared group-deletion sensitivity",
      subtitle = "No participant or item is excluded automatically."
    ) +
    theme_gp3bayes()
}

#' Plot Random-Slope Sensitivity
#'
#' @param x A random-slope sensitivity object or estimand-sensitivity table.
#' @return A `ggplot`.
#' @export
plot_random_slope_sensitivity <- function(x) {
  d <- if (inherits(x, "gp3bayes_random_slope_sensitivity")) {
    random_slope_sensitivity_table(x)
  } else {
    x
  }
  plot_estimand_sensitivity_gg(d) +
    ggplot2::labs(
      title = "Random-intercept versus random-slope sensitivity",
      subtitle = "The comparison does not select a structure automatically."
    )
}

#' Plot Power-Scale Sensitivity with ggplot2
#'
#' @param x A power-scale sensitivity object or its table.
#' @return A `ggplot`.
#' @export
plot_powerscale_sensitivity_gg <- function(x) {
  .gp3g_require("ggplot2", "plot power-scale sensitivity")
  d <- .gp3rs_plot_table(
    x,
    "gp3bayes_powerscale_sensitivity",
    powerscale_sensitivity_table
  )
  required <- c("variable", "prior", "likelihood")
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3rs_stop(
      "`x` must expose `variable`, `prior`, and `likelihood` sensitivity columns."
    )
  }
  long <- rbind(
    data.frame(
      variable = d$variable,
      component = "prior",
      sensitivity = d$prior,
      stringsAsFactors = FALSE
    ),
    data.frame(
      variable = d$variable,
      component = "likelihood",
      sensitivity = d$likelihood,
      stringsAsFactors = FALSE
    )
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = variable, y = sensitivity, fill = component)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Local power-scale sensitivity",
      fill = NULL,
      title = "Prior and likelihood power-scale sensitivity"
    ) +
    theme_gp3bayes()
}

.gp3rs_sbc_plot <- function(fun, x, variables, dots, purpose) {
  .gp3rs_class(x, "gp3bayes_sbc_result")
  .gp3p_require("SBC", purpose)
  args <- c(list(x$raw), dots)
  if (!is.null(variables)) args$variables <- variables
  .gp3p_call_supported(fun, args)
}

#' SBC Rank-Histogram Plot
#'
#' @param x A `gp3bayes_sbc_result`.
#' @param variables Optional variables.
#' @param ... Arguments passed to SBC.
#' @return A plot object returned by SBC.
#' @export
plot_sbc_rank_gg <- function(x, variables = NULL, ...) {
  .gp3rs_sbc_plot(
    SBC::plot_rank_hist,
    x,
    variables,
    list(...),
    "plot SBC rank histograms"
  )
}

#' SBC ECDF-Difference Plot
#'
#' @inheritParams plot_sbc_rank_gg
#' @return A plot object returned by SBC.
#' @export
plot_sbc_ecdf_gg <- function(x, variables = NULL, ...) {
  .gp3rs_sbc_plot(
    SBC::plot_ecdf_diff,
    x,
    variables,
    list(...),
    "plot SBC ECDF diagnostics"
  )
}

#' SBC Coverage Plot
#'
#' @inheritParams plot_sbc_rank_gg
#' @return A plot object returned by SBC.
#' @export
plot_sbc_coverage_gg <- function(x, variables = NULL, ...) {
  .gp3rs_sbc_plot(
    SBC::plot_coverage,
    x,
    variables,
    list(...),
    "plot SBC empirical coverage"
  )
}

#' SBC Simulated-versus-Estimated Plot
#'
#' @inheritParams plot_sbc_rank_gg
#' @return A plot object returned by SBC.
#' @export
plot_sbc_simulated_vs_estimated_gg <- function(x, variables = NULL, ...) {
  .gp3rs_sbc_plot(
    SBC::plot_sim_estimated,
    x,
    variables,
    list(...),
    "plot SBC simulated-versus-estimated values"
  )
}

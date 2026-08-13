# ggplot adapters for existing governance and evidence objects

.gp3eg_df <- function(x, class_name) {
  if (!inherits(x, class_name)) {
    .gp3p_stop("`x` must inherit from `", class_name, "`.")
  }
  as.data.frame(x)
}

#' Sensitivity-Suite Table
#'
#' @param x A `gp3bayes_sensitivity_suite`.
#' @return The component-level sensitivity table.
#' @export
sensitivity_suite_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_sensitivity_suite")
}

#' Model-Evidence Table
#'
#' @param x A `gp3bayes_model_evidence`.
#' @return The model-evidence inventory table.
#' @export
model_evidence_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_model_evidence")
}

#' Backend-Parity Table
#'
#' @param x A `gp3bayes_backend_parity_audit`.
#' @return The parameter-level backend-parity table.
#' @export
backend_parity_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_backend_parity_audit")
}

#' Backend-Environment Table
#'
#' @param x A `gp3bayes_backend_environment`.
#' @return Backend environment checks.
#' @export
backend_environment_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_backend_environment")
}

#' Manifest-Comparison Table
#'
#' @param x A `gp3bayes_manifest_comparison`.
#' @return Manifest component comparisons.
#' @export
manifest_comparison_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_manifest_comparison")
}

#' Schema-Comparison Table
#'
#' @param x A `gp3bayes_schema_comparison`.
#' @return Structural schema comparisons.
#' @export
schema_comparison_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_schema_comparison")
}

#' Design-Support Table
#'
#' @param x A `gp3bayes_design_support_audit`.
#' @return The design-support component table.
#' @export
design_support_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_design_support_audit")
}

#' Missingness-Audit Table
#'
#' @param x A `gp3bayes_missingness_audit`.
#' @return Column-level missingness diagnostics.
#' @export
missingness_audit_table <- function(x) {
  .gp3eg_df(x, "gp3bayes_missingness_audit")
}

#' Plot a Sensitivity Suite with ggplot2
#'
#' @param x A `gp3bayes_sensitivity_suite`.
#' @return A `ggplot`.
#' @export
plot_sensitivity_suite_gg <- function(x) {
  .gp3g_require("ggplot2", "plot a sensitivity suite")
  d <- sensitivity_suite_table(x)
  if (!nrow(d)) .gp3p_stop("The sensitivity suite contains no components.")
  categories <- c("not_assessed", "error", "fail", "review", "completed", "pass")
  d$status_code <- match(d$status, categories)
  d$status_code[is.na(d$status_code)] <- match("completed", categories)
  d$component <- factor(d$component, levels = rev(d$component))

  ggplot2::ggplot(d, ggplot2::aes(x = component, y = status_code)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(categories),
      labels = categories
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Recorded component status",
      title = "Sensitivity-suite evidence"
    ) +
    theme_gp3bayes()
}

#' Plot Model-Evidence Availability with ggplot2
#'
#' @param x A `gp3bayes_model_evidence`.
#' @return A `ggplot`.
#' @export
plot_model_evidence_gg <- function(x) {
  .gp3g_require("ggplot2", "plot model evidence")
  d <- model_evidence_table(x)
  if (!all(c("component", "available") %in% names(d))) {
    .gp3p_stop("The model-evidence table has an unsupported structure.")
  }
  d$component <- factor(d$component, levels = rev(d$component))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = component, y = as.integer(available))
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Unavailable", "Available")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Model-evidence inventory",
      subtitle = "Availability does not constitute an automatic adequacy verdict."
    ) +
    theme_gp3bayes()
}

#' Plot Backend Parity with ggplot2
#'
#' @param x A `gp3bayes_backend_parity_audit`.
#' @return A `ggplot`.
#' @export
plot_backend_parity_gg <- function(x) {
  .gp3g_require("ggplot2", "plot backend parity")
  d <- backend_parity_table(x)
  required <- c("variable", "rstan_mean", "cmdstanr_mean", "status")
  if (!all(required %in% names(d))) {
    .gp3p_stop("The backend-parity table has an unsupported structure.")
  }
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(y = variable)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = rstan_mean, xend = cmdstanr_mean, yend = variable)
    ) +
    ggplot2::geom_point(ggplot2::aes(x = rstan_mean, shape = "rstan")) +
    ggplot2::geom_point(ggplot2::aes(x = cmdstanr_mean, shape = "cmdstanr")) +
    ggplot2::labs(
      x = "Posterior mean",
      y = NULL,
      shape = "Backend",
      title = "Backend posterior-summary parity",
      subtitle = "Parity assesses computational consistency, not model adequacy."
    ) +
    theme_gp3bayes()
}

#' Plot Backend Environment Checks with ggplot2
#'
#' @param x A `gp3bayes_backend_environment`.
#' @return A `ggplot`.
#' @export
plot_backend_environment_gg <- function(x) {
  .gp3g_require("ggplot2", "plot backend-environment checks")
  d <- backend_environment_table(x)
  required <- c("check", "status")
  if (!all(required %in% names(d))) {
    .gp3p_stop("The backend-environment table has an unsupported structure.")
  }
  d$check <- factor(d$check, levels = rev(d$check))
  d$value <- as.integer(d$status == "pass")
  ggplot2::ggplot(d, ggplot2::aes(x = check, y = value)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Not ready", "Pass")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Backend environment checks"
    ) +
    theme_gp3bayes()
}

#' Plot Manifest Differences with ggplot2
#'
#' @param x A `gp3bayes_manifest_comparison`.
#' @return A `ggplot`.
#' @export
plot_manifest_comparison_gg <- function(x) {
  .gp3g_require("ggplot2", "plot manifest differences")
  d <- manifest_comparison_table(x)
  if (!all(c("component", "identical") %in% names(d))) {
    .gp3p_stop("The manifest-comparison table has an unsupported structure.")
  }
  d$component <- factor(d$component, levels = rev(d$component))
  d$changed <- as.integer(!d$identical)
  ggplot2::ggplot(d, ggplot2::aes(x = component, y = changed)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Identical", "Changed")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Analysis-manifest differences",
      subtitle = "Differences are provenance differences, not automatic invalidity."
    ) +
    theme_gp3bayes()
}

#' Plot Schema Differences with ggplot2
#'
#' @param x A `gp3bayes_schema_comparison`.
#' @return A `ggplot`.
#' @export
plot_schema_comparison_gg <- function(x) {
  .gp3g_require("ggplot2", "plot schema differences")
  d <- schema_comparison_table(x)
  if (!all(c("path", "status") %in% names(d))) {
    .gp3p_stop("The schema-comparison table has an unsupported structure.")
  }
  d$path <- factor(d$path, levels = rev(d$path))
  d$changed <- as.integer(d$status == "review")
  ggplot2::ggplot(d, ggplot2::aes(x = path, y = changed)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Compatible", "Review")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "gp3bayes object-schema comparison"
    ) +
    theme_gp3bayes()
}

#' Plot Design-Support Components with ggplot2
#'
#' @param x A `gp3bayes_design_support_audit`.
#' @return A `ggplot`.
#' @export
plot_design_support_gg <- function(x) {
  .gp3g_require("ggplot2", "plot design-support components")
  d <- design_support_table(x)
  if (!all(c("component", "status") %in% names(d))) {
    .gp3p_stop("The design-support table has an unsupported structure.")
  }
  categories <- c("fail", "review", "pass")
  d$status_code <- match(d$status, categories)
  d$component <- factor(d$component, levels = rev(d$component))
  ggplot2::ggplot(d, ggplot2::aes(x = component, y = status_code)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(categories),
      labels = categories
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Audit status",
      title = "Pre-fit design-support audit"
    ) +
    theme_gp3bayes()
}

#' Plot Missingness Fractions with ggplot2
#'
#' @param x A `gp3bayes_missingness_audit`.
#' @return A `ggplot`.
#' @export
plot_missingness_gg <- function(x) {
  .gp3g_require("ggplot2", "plot missingness diagnostics")
  d <- missingness_audit_table(x)
  if (!all(c("column", "fraction_missing", "status") %in% names(d))) {
    .gp3p_stop("The missingness-audit table has an unsupported structure.")
  }
  d$column <- factor(d$column, levels = rev(d$column))
  ggplot2::ggplot(d, ggplot2::aes(x = column, y = fraction_missing)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Fraction missing",
      title = "Declared-column missingness"
    ) +
    theme_gp3bayes()
}

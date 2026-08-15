
utils::globalVariables(c(
  ".event_time", ".condition", ".pupil_model", ".series_id",
  "status", "metric", "pupil", "estimate", "lower", "upper",
  ".plot_group", ".label", "lag", "acf", "replicated_mean",
  "observed_mean", "mean_residual", "scenario_id", "axis", "value",
  "available", "domain", "statistic", "observed", "replicated_median",
  "observed_sd", "replicated_median_sd", "grouping",
  "missing_pupil_proportion", "blink_proportion",
  "interpolated_proportion", "quantity", "se", "label"
))

.gp3p_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .gp3p_stop("Package `ggplot2` is required for this plot.")
  }
}

#' Plot pupil readiness evidence
#' @param x Pupil readiness audit.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_readiness <- function(x) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_readiness")) {
    .gp3p_stop("`x` must be a pupil readiness audit.")
  }
  tab <- x$summary
  tab$metric <- factor(tab$metric, levels = rev(tab$metric))
  ggplot2::ggplot(tab, ggplot2::aes(x = status, y = metric)) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::labs(
      x = "Audit status", y = NULL,
      title = "Pupil readiness evidence",
      subtitle = "Review signals are not automatic exclusions"
    ) +
    theme_gp3bayes()
}

#' Plot observed pupil trajectories
#' @param x Prepared pupil object.
#' @param summary Whether to plot condition means rather than individual
#'   participant-trial traces.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_observed_trajectory <- function(x, summary = TRUE) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_prepared")) {
    .gp3p_stop("`x` must be a prepared pupil object.")
  }
  summary <- .gp3p_flag(summary, "summary")
  d <- x$data
  if (summary) {
    condition <- if (".condition" %in% names(d)) {
      as.character(d$.condition)
    } else {
      rep("all", nrow(d))
    }
    group <- interaction(d$.event_time, condition, drop = TRUE)
    idx <- split(seq_len(nrow(d)), group)
    tab <- do.call(
      rbind,
      lapply(
        idx,
        function(i) data.frame(
          .event_time = d$.event_time[i[1L]],
          .condition = condition[i[1L]],
          pupil = mean(d$.pupil_model[i], na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      )
    )
    tab <- tab[is.finite(tab$pupil), , drop = FALSE]
    p <- ggplot2::ggplot(
      tab,
      ggplot2::aes(
        x = .event_time, y = pupil, linetype = .condition,
        group = .condition
      )
    ) + ggplot2::geom_line(linewidth = 0.7)
  } else {
    tab <- d
    p <- ggplot2::ggplot(
      tab,
      ggplot2::aes(x = .event_time, y = .pupil_model, group = .series_id)
    ) + ggplot2::geom_line(alpha = 0.2, linewidth = 0.35)
  }
  p + ggplot2::labs(
    x = "Event-relative time (seconds)",
    y = paste0("Pupil response (", x$model_unit, ")"),
    linetype = "Condition",
    title = "Observed pupil time course"
  ) + theme_gp3bayes()
}

#' Plot posterior pupil trajectories or condition differences
#' @param x Pupil trajectory or condition-contrast object.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_posterior_trajectory <- function(x) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_trajectory")) {
    .gp3p_stop("`x` must be a pupil trajectory.")
  }
  tab <- x$table
  group_col <- if (".condition" %in% names(tab)) {
    ".condition"
  } else if ("contrast" %in% names(tab)) {
    "contrast"
  } else {
    NULL
  }

  if (is.null(group_col)) {
    p <- ggplot2::ggplot(
      tab, ggplot2::aes(x = .event_time, y = estimate)
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20
      ) +
      ggplot2::geom_line(linewidth = 0.7)
  } else {
    tab$.plot_group <- as.character(tab[[group_col]])
    p <- ggplot2::ggplot(
      tab,
      ggplot2::aes(
        x = .event_time, y = estimate,
        linetype = .plot_group, group = .plot_group
      )
    ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper, group = .plot_group),
        alpha = 0.16
      ) +
      ggplot2::geom_line(linewidth = 0.7)
  }
  unit <- .gp3p_null_or(x$unit, "model scale")
  p + ggplot2::labs(
    x = "Event-relative time (seconds)",
    y = paste0("Posterior pupil response (", unit, ")"),
    linetype = if (identical(group_col, ".condition")) "Condition" else "Contrast",
    title = if (inherits(x, "gp3bayes_pupil_condition_contrast")) {
      "Posterior pupil condition difference"
    } else {
      "Posterior pupil trajectory"
    }
  ) + theme_gp3bayes()
}

#' Plot declared-window pupil estimands
#' @param x Pupil estimand object.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_estimand <- function(x) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_estimand")) {
    .gp3p_stop("`x` must be a pupil estimand.")
  }
  tab <- x$table
  label <- if (".condition" %in% names(tab)) {
    as.character(tab$.condition)
  } else if ("contrast" %in% names(tab)) {
    as.character(tab$contrast)
  } else {
    seq_len(nrow(tab))
  }
  tab$.label <- factor(label, levels = unique(label))
  ggplot2::ggplot(tab, ggplot2::aes(x = .label, y = estimate)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lower, ymax = upper)) +
    ggplot2::labs(
      x = NULL,
      y = paste0(
        .gp3p_null_or(x$estimand, "Pupil estimand"),
        if (!is.null(x$unit)) paste0(" (", x$unit, ")") else ""
      ),
      title = "Declared pupil estimand",
      subtitle = if (!is.null(x$window)) {
        paste("Window:", paste(x$window, collapse = " to "), "seconds")
      } else NULL
    ) +
    theme_gp3bayes()
}

#' Plot pupil posterior-predictive evidence
#'
#' Uses one consolidated plotting interface for trajectory, residual,
#' feature, autocorrelation, heterogeneity, or measurement-context PPC
#' evidence.
#'
#' @param x Pupil PPC object.
#' @param component Evidence component to plot.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_ppc <- function(
    x,
    component = c(
      "trajectory", "residuals", "features", "autocorrelation",
      "heterogeneity", "measurement_context"
    )) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_ppc")) {
    .gp3p_stop("`x` must be a pupil PPC object.")
  }
  component <- match.arg(component)

  if (identical(component, "trajectory")) {
    tab <- x$trajectory
    return(
      ggplot2::ggplot(
        tab,
        ggplot2::aes(
          x = .event_time, group = .condition, linetype = .condition
        )
      ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = lower, ymax = upper, group = .condition),
          alpha = 0.16
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = replicated_mean), linewidth = 0.7
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = observed_mean), size = 1.2
        ) +
        ggplot2::labs(
          x = "Event-relative time (seconds)",
          y = paste0("Pupil response (", x$unit, ")"),
          linetype = "Condition",
          title = "Pupil posterior-predictive trajectory",
          subtitle = "Points: observed means; lines/bands: posterior predictive"
        ) +
        theme_gp3bayes()
    )
  }

  if (identical(component, "residuals")) {
    tab <- x$residual_trajectory
    return(
      ggplot2::ggplot(
        tab,
        ggplot2::aes(
          x = .event_time, y = mean_residual,
          group = .condition, linetype = .condition
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::labs(
          x = "Event-relative time (seconds)",
          y = paste0("Mean residual (", x$unit, ")"),
          linetype = "Condition",
          title = "Pupil residual trajectory",
          subtitle = "Temporal residual evidence; no adequacy certification"
        ) +
        theme_gp3bayes()
    )
  }

  if (identical(component, "features")) {
    tab <- x$features
    long <- rbind(
      data.frame(
        statistic = tab$statistic, source = "Observed",
        value = tab$observed, stringsAsFactors = FALSE
      ),
      data.frame(
        statistic = tab$statistic, source = "Replicated median",
        value = tab$replicated_median, stringsAsFactors = FALSE
      )
    )
    return(
      ggplot2::ggplot(long, ggplot2::aes(x = source, y = value)) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::facet_wrap(~ statistic, scales = "free_y") +
        ggplot2::labs(
          x = NULL, y = "Feature value",
          title = "Pupil posterior-predictive feature checks",
          subtitle = "Peak/latency/AUC are descriptive PPC features, not selected effects"
        ) +
        theme_gp3bayes()
    )
  }

  if (identical(component, "autocorrelation")) {
    tab <- x$autocorrelation
    long <- data.frame(
      source = c("Observed", "Replicated median"),
      value = c(tab$observed, tab$replicated_median),
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(long, ggplot2::aes(x = source, y = value)) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::labs(
          x = NULL, y = "Mean within-series lag-1 correlation",
          title = "Pupil posterior-predictive serial structure"
        ) +
        theme_gp3bayes()
    )
  }

  if (identical(component, "heterogeneity")) {
    tab <- x$heterogeneity
    long <- rbind(
      data.frame(
        grouping = tab$grouping, source = "Observed",
        value = tab$observed_sd, stringsAsFactors = FALSE
      ),
      data.frame(
        grouping = tab$grouping, source = "Replicated median",
        value = tab$replicated_median_sd, stringsAsFactors = FALSE
      )
    )
    return(
      ggplot2::ggplot(long, ggplot2::aes(x = source, y = value)) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::facet_wrap(~ grouping, scales = "free_y") +
        ggplot2::labs(
          x = NULL, y = "SD of group means",
          title = "Pupil posterior-predictive heterogeneity"
        ) +
        theme_gp3bayes()
    )
  }

  tab <- x$measurement_context
  long <- rbind(
    data.frame(
      .event_time = tab$.event_time, .condition = tab$.condition,
      metric = "Missing pupil", value = tab$missing_pupil_proportion,
      stringsAsFactors = FALSE
    ),
    data.frame(
      .event_time = tab$.event_time, .condition = tab$.condition,
      metric = "Blink", value = tab$blink_proportion,
      stringsAsFactors = FALSE
    ),
    data.frame(
      .event_time = tab$.event_time, .condition = tab$.condition,
      metric = "Interpolated", value = tab$interpolated_proportion,
      stringsAsFactors = FALSE
    )
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .event_time, y = value,
      linetype = .condition, group = .condition
    )
  ) +
    ggplot2::geom_line(linewidth = 0.65, na.rm = TRUE) +
    ggplot2::facet_wrap(~ metric) +
    ggplot2::labs(
      x = "Event-relative time (seconds)",
      y = "Proportion",
      linetype = "Condition",
      title = "Pupil measurement context",
      subtitle = "Missingness, blink, and interpolation overlays are evidence only"
    ) +
    theme_gp3bayes()
}

#' Plot pupil residual autocorrelation
#' @param x Pupil diagnostics object or residual ACF table.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_residual_acf <- function(x) {
  .gp3p_require_ggplot2()
  tab <- if (inherits(x, "gp3bayes_pupil_diagnostics")) {
    x$residual_acf
  } else {
    x
  }
  if (!is.data.frame(tab) || !all(c("lag", "acf") %in% names(tab))) {
    .gp3p_stop("`x` must contain residual ACF columns `lag` and `acf`.")
  }
  ggplot2::ggplot(tab, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = lag, y = 0, yend = acf), linewidth = 0.55
    ) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::labs(
      x = "Lag (samples)", y = "Mean residual autocorrelation",
      title = "Within-trial residual autocorrelation"
    ) +
    theme_gp3bayes()
}

#' Plot pupil validation evidence
#' @param x Pupil validation object.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_validation <- function(x) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_validation")) {
    .gp3p_stop("`x` must be a pupil validation object.")
  }
  tab <- x$table
  if (!isTRUE(x$executed)) {
    tab$label <- paste(tab$target, tab$strategy, sep = "\n")
    tab$value <- 1
    return(
      ggplot2::ggplot(tab, ggplot2::aes(x = label, y = value)) +
        ggplot2::geom_point(size = 2.4) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Declared pupil validation target",
          subtitle = "Plan only; refitting not executed"
        ) +
        theme_gp3bayes()
    )
  }

  if (all(c("quantity", "estimate", "se") %in% names(tab))) {
    return(
      ggplot2::ggplot(tab, ggplot2::aes(x = quantity, y = estimate)) +
        ggplot2::geom_pointrange(
          ggplot2::aes(ymin = estimate - se, ymax = estimate + se)
        ) +
        ggplot2::labs(
          x = NULL, y = "Cross-validated estimate (\u00b11 SE)",
          title = paste("Pupil validation:", x$target),
          subtitle = "Target-specific predictive evidence"
        ) +
        theme_gp3bayes()
    )
  }

  numeric_cols <- names(tab)[vapply(tab, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("n_train", "n_test"))
  if (!length(numeric_cols)) {
    .gp3p_stop("Executed validation table has no plottable numeric metric.")
  }
  long <- do.call(
    rbind,
    lapply(
      numeric_cols,
      function(nm) data.frame(
        metric = nm, value = tab[[nm]], stringsAsFactors = FALSE
      )
    )
  )
  ggplot2::ggplot(long, ggplot2::aes(x = metric, y = value)) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::labs(
      x = NULL, y = "Observed validation quantity",
      title = paste("Pupil validation:", x$target),
      subtitle = "Future-segment predictive evidence; no validity certificate"
    ) +
    theme_gp3bayes()
}

#' Plot pupil sensitivity scenarios or result comparisons
#' @param x Pupil sensitivity suite or comparison.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_sensitivity <- function(x) {
  .gp3p_require_ggplot2()
  tab <- pupil_sensitivity_table(x)
  if (inherits(x, "gp3bayes_pupil_sensitivity")) {
    if (!nrow(tab)) .gp3p_stop("No sensitivity scenarios were declared.")
    tab$axis <- factor(tab$axis, levels = unique(tab$axis))
    return(
      ggplot2::ggplot(tab, ggplot2::aes(x = scenario_id, y = axis)) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::labs(
          x = "Scenario", y = NULL,
          title = "Declared pupil sensitivity suite",
          subtitle = "No scenario is automatically preferred"
        ) +
        theme_gp3bayes()
    )
  }
  if (!all(c("scenario_id", "estimate") %in% names(tab))) {
    .gp3p_stop("Sensitivity comparison lacks scenario and estimate columns.")
  }
  p <- ggplot2::ggplot(
    tab, ggplot2::aes(x = scenario_id, y = estimate)
  ) + ggplot2::geom_point(size = 2.2)
  if (all(c("lower", "upper") %in% names(tab))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper), width = 0.1
    )
  }
  p + ggplot2::labs(
    x = "Scenario", y = "Posterior estimand",
    title = "Pupil sensitivity comparison",
    subtitle = "Declared alternatives; no automatic selection"
  ) +
    theme_gp3bayes()
}

#' Plot gaze/PFE and luminance measurement-context evidence
#' @param x A pupil measurement audit.
#' @return A `ggplot` object.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
plot_pupil_measurement_audit <- function(x) {
  .gp3p_require_ggplot2()
  if (!inherits(x, "gp3bayes_pupil_measurement_audit")) {
    .gp3p_stop("`x` must be a pupil measurement audit.")
  }
  tab <- x$table
  tab$domain <- factor(tab$domain, levels = rev(tab$domain))
  ggplot2::ggplot(tab, ggplot2::aes(x = available, y = domain)) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::labs(
      x = "Information available", y = NULL,
      title = "Pupil measurement-context audit",
      subtitle = "Gaze/PFE, luminance, baseline, and data-loss evidence only"
    ) +
    theme_gp3bayes()
}

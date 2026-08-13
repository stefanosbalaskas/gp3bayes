# High-level publication bundle helpers

.gp3a_capture <- function(expr) {
  tryCatch(
    list(ok = TRUE, value = force(expr), error = NULL),
    error = function(e) {
      list(ok = FALSE, value = NULL, error = conditionMessage(e))
    }
  )
}

#' Create a Structured Post-Fit Analysis Bundle
#'
#' Collects reusable posterior, diagnostic, prediction, calibration, scoring,
#' and optionally PSIS-LOO tables without making an automatic adequacy or model
#' selection decision.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param newdata Optional prediction data.
#' @param ndraws Posterior draws used for prediction-facing components.
#' @param include_group_effects Whether prediction summaries include recorded
#'   group-level effects.
#' @param include_loo Whether PSIS-LOO is computed.
#'
#' @return A `gp3bayes_analysis_bundle`.
#' @export
create_analysis_bundle <- function(
  fit,
  newdata = NULL,
  ndraws = 1000L,
  include_group_effects = FALSE,
  include_loo = FALSE
) {
  .gp3p_validate_fit(fit)
  include_loo <- .gp3pr_flag(include_loo, "include_loo")

  components <- list(
    posterior = .gp3a_capture(
      posterior_interval_table(fit, regex = "^(b_|sd_|cor_|sigma$)")
    ),
    mcmc = .gp3a_capture(summarise_mcmc_quality(fit)),
    prediction_support = .gp3a_capture(
      audit_prediction_support(
        fit,
        if (is.null(newdata)) .gp3p_training_data(fit) else newdata
      )
    ),
    expected_prediction = .gp3a_capture(
      predict_model(
        fit,
        newdata = newdata,
        type = if (fit$family == "duration") "median" else "expected",
        include_group_effects = include_group_effects,
        ndraws = ndraws
      )
    ),
    predictive = .gp3a_capture(
      predict_model(
        fit,
        newdata = newdata,
        type = "predictive",
        include_group_effects = include_group_effects,
        ndraws = ndraws,
        seed = 1L
      )
    ),
    group_effects = .gp3a_capture(group_effect_table(fit)),
    variance_components = .gp3a_capture(variance_component_table(fit))
  )

  if (components$expected_prediction$ok &&
      !is.null(components$expected_prediction$value$observed)) {
    components$scores <- .gp3a_capture(
      if (fit$family == "binary") {
        binary_prediction_scores(components$expected_prediction$value)
      } else {
        duration_prediction_scores(components$expected_prediction$value)
      }
    )
  }

  if (components$predictive$ok &&
      !is.null(components$predictive$value$observed)) {
    components$coverage <- .gp3a_capture(
      predictive_coverage_table(components$predictive$value)
    )
    if (fit$family == "duration") {
      components$quantile_calibration <- .gp3a_capture(
        duration_quantile_calibration(components$predictive$value)
      )
    }
  }

  if (fit$family == "binary" &&
      components$expected_prediction$ok &&
      !is.null(components$expected_prediction$value$observed)) {
    components$calibration <- .gp3a_capture(
      binary_calibration_table(components$expected_prediction$value)
    )
  }

  if (include_loo) {
    components$loo <- .gp3a_capture(compute_psis_loo(fit, cores = 1L))
  }

  component_status <- data.frame(
    component = names(components),
    available = vapply(components, `[[`, logical(1L), "ok"),
    error = vapply(
      components,
      function(z) if (is.null(z$error)) "" else z$error,
      character(1L)
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      bundle_version = "0.3",
      family = fit$family,
      fit = fit,
      components = components,
      status = component_status,
      include_loo = include_loo,
      automatic_decision = FALSE,
      interpretation = paste(
        "The bundle collects post-fit evidence for inspection and reporting.",
        "It does not automatically establish adequacy, robustness, or causal validity."
      )
    ),
    class = "gp3bayes_analysis_bundle"
  )
}

#' @export
print.gp3bayes_analysis_bundle <- function(x, ...) {
  cat("\ngp3bayes analysis bundle\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Components: ", nrow(x$status), "\n", sep = "")
  cat(" Available: ", sum(x$status$available), "\n", sep = "")
  cat(" Automatic decision: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_analysis_bundle <- function(x, ...) x$status

#' Analysis-Bundle Status Table
#'
#' @param x A `gp3bayes_analysis_bundle`.
#'
#' @return Component availability and captured errors.
#' @export
analysis_bundle_table <- function(x) {
  if (!inherits(x, "gp3bayes_analysis_bundle")) {
    .gp3p_stop("`x` must be a gp3bayes analysis bundle.")
  }
  x$status
}

#' Extract Publication Tables from an Analysis Bundle
#'
#' @param x A `gp3bayes_analysis_bundle`.
#'
#' @return A named list of data frames suitable for downstream formatting.
#' @export
create_publication_table_set <- function(x) {
  if (!inherits(x, "gp3bayes_analysis_bundle")) {
    .gp3p_stop("`x` must be a gp3bayes analysis bundle.")
  }
  out <- list()
  for (nm in names(x$components)) {
    z <- x$components[[nm]]
    if (!isTRUE(z$ok)) next
    value <- z$value
    table <- if (is.data.frame(value)) {
      value
    } else if (inherits(value, "gp3bayes_prediction")) {
      value$summary
    } else if (inherits(value, "gp3bayes_mcmc_quality")) {
      value$issues
    } else if (inherits(value, "gp3bayes_prediction_support")) {
      value$table
    } else if (inherits(value, "gp3bayes_psis_loo")) {
      loo_summary_table(value)
    } else {
      NULL
    }
    if (!is.null(table)) out[[nm]] <- table
  }
  out
}

#' Create Publication Figures from an Analysis Bundle
#'
#' Produces only figures supported by available bundle components.
#'
#' @param x A `gp3bayes_analysis_bundle`.
#'
#' @return A `gp3bayes_figure_set`.
#' @export
create_analysis_figure_set <- function(x) {
  if (!inherits(x, "gp3bayes_analysis_bundle")) {
    .gp3p_stop("`x` must be a gp3bayes analysis bundle.")
  }
  .gp3g_require("ggplot2", "create analysis figures")
  plots <- list()

  if (isTRUE(x$components$posterior$ok)) {
    plots$posterior_intervals <- plot_posterior_intervals(
      x$fit,
      regex = "^(b_|sd_|sigma$)"
    )
  }
  if (isTRUE(x$components$mcmc$ok)) {
    plots$mcmc_quality <- plot_mcmc_quality(x$components$mcmc$value)
  }
  if (isTRUE(x$components$prediction_support$ok)) {
    plots$prediction_support <- plot_prediction_support(
      x$components$prediction_support$value
    )
  }
  if (isTRUE(x$components$expected_prediction$ok)) {
    plots$prediction_intervals <- plot_prediction_intervals(
      x$components$expected_prediction$value
    )
  }
  if ("calibration" %in% names(x$components) &&
      isTRUE(x$components$calibration$ok)) {
    plots$calibration <- plot_binary_calibration(x$components$calibration$value)
  }
  if ("quantile_calibration" %in% names(x$components) &&
      isTRUE(x$components$quantile_calibration$ok)) {
    plots$quantile_calibration <- plot_duration_quantile_calibration(
      x$components$quantile_calibration$value
    )
  }
  if ("coverage" %in% names(x$components) &&
      isTRUE(x$components$coverage$ok)) {
    plots$predictive_coverage <- plot_predictive_coverage(
      x$components$coverage$value
    )
  }
  if (isTRUE(x$components$group_effects$ok)) {
    plots$group_effects <- plot_group_effects(x$components$group_effects$value)
  }
  if (isTRUE(x$components$variance_components$ok)) {
    plots$variance_components <- plot_variance_components(
      x$components$variance_components$value
    )
  }
  if ("loo" %in% names(x$components) && isTRUE(x$components$loo$ok)) {
    plots$loo_influence <- plot_loo_influence(x$components$loo$value)
  }

  if (!length(plots)) {
    .gp3p_stop("No bundle components could be converted to figures.")
  }
  do.call(
    create_figure_set,
    c(plots, list(title = "gp3bayes analysis figures"))
  )
}

#' Write an Analysis-Bundle Markdown Report
#'
#' @param x A `gp3bayes_analysis_bundle`.
#' @param file Explicit output file path.
#'
#' @return Invisibly, the normalized output path.
#' @export
write_analysis_bundle_report <- function(x, file) {
  if (!inherits(x, "gp3bayes_analysis_bundle")) {
    .gp3p_stop("`x` must be a gp3bayes analysis bundle.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3p_stop("`file` must be one explicit output path.")
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tables <- create_publication_table_set(x)

  lines <- c(
    "# gp3bayes post-fit analysis bundle",
    "",
    paste0("- Family: `", x$family, "`"),
    paste0("- Components available: ", sum(x$status$available), "/", nrow(x$status)),
    "- Automatic adequacy/model-selection decision: `FALSE`",
    "",
    "## Component status",
    "",
    "```",
    utils::capture.output(print(x$status, row.names = FALSE)),
    "```",
    ""
  )

  for (nm in names(tables)) {
    lines <- c(
      lines,
      paste0("## ", gsub("_", " ", nm)),
      "",
      "```",
      utils::capture.output(print(tables[[nm]], row.names = FALSE)),
      "```",
      ""
    )
  }
  lines <- c(
    lines,
    "## Interpretation boundary",
    "",
    x$interpretation,
    ""
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

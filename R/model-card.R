# Structured model cards and reporting inventory

.gp3mc_status <- function(value) {
  if (is.null(value)) return("not_available")
  if (inherits(value, "error")) return("error")
  "available"
}

#' Create a gp3bayes Model Card
#'
#' Creates a compact, structured record of model identity, computational
#' diagnostics, prediction evidence, provenance, and interpretation boundaries.
#' The card is documentation; it does not issue a model-adequacy certificate.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param analysis_bundle Optional `gp3bayes_analysis_bundle`.
#' @param manifest Optional `gp3bayes_analysis_manifest`.
#' @param label Optional human-readable label.
#'
#' @return A `gp3bayes_model_card`.
#' @export
create_model_card <- function(
  fit,
  analysis_bundle = NULL,
  manifest = NULL,
  label = NULL
) {
  .gp3p_validate_fit(fit)

  if (!is.null(analysis_bundle) &&
      !inherits(analysis_bundle, "gp3bayes_analysis_bundle")) {
    .gp3p_stop("`analysis_bundle` must be NULL or a gp3bayes analysis bundle.")
  }
  if (!is.null(manifest) &&
      !inherits(manifest, "gp3bayes_analysis_manifest")) {
    .gp3p_stop("`manifest` must be NULL or a gp3bayes analysis manifest.")
  }
  if (!is.null(label) &&
      (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label))) {
    .gp3p_stop("`label` must be NULL or one non-empty character value.")
  }

  diagnosis <- tryCatch(
    diagnose_model_fit(fit),
    error = function(e) e
  )
  workflow <- tryCatch(
    model_workflow_status(fit),
    error = function(e) e
  )

  evidence <- data.frame(
    component = c(
      "model_fit",
      "model_diagnosis",
      "workflow_status",
      "analysis_bundle",
      "analysis_manifest"
    ),
    status = c(
      "available",
      .gp3mc_status(diagnosis),
      .gp3mc_status(workflow),
      .gp3mc_status(analysis_bundle),
      .gp3mc_status(manifest)
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      card_version = "0.3",
      label = label,
      family = fit$family,
      model_family = fit$specification$model_family %||%
        fit$specification$contract$model_family %||%
        NA_character_,
      formula = paste(deparse(fit$specification$formula), collapse = " "),
      sampling_backend = fit$sampling_backend %||%
        fit$backend_interface %||%
        NA_character_,
      sampling = fit$sampling,
      package_versions = fit$package_versions,
      diagnosis = diagnosis,
      workflow = workflow,
      analysis_bundle = analysis_bundle,
      manifest = manifest,
      evidence = evidence,
      model_adequacy_certified = FALSE,
      causal_identification_certified = FALSE,
      automatic_model_selection = FALSE,
      interpretation = paste(
        "This model card records analysis identity and available evidence.",
        "It does not certify model adequacy, causal identification, or substantive validity."
      )
    ),
    class = "gp3bayes_model_card"
  )
}

#' @export
print.gp3bayes_model_card <- function(x, ...) {
  cat("\ngp3bayes model card\n")
  if (!is.null(x$label)) cat(" Label: ", x$label, "\n", sep = "")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Backend: ", x$sampling_backend, "\n", sep = "")
  cat(" Evidence components: ", nrow(x$evidence), "\n", sep = "")
  cat(" Model adequacy certified: FALSE\n")
  cat(" Causal identification certified: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_model_card <- function(x, ...) {
  x$evidence
}

#' Model-Card Evidence Table
#'
#' @param x A `gp3bayes_model_card`.
#' @return The model-card evidence inventory.
#' @export
model_card_table <- function(x) {
  if (!inherits(x, "gp3bayes_model_card")) {
    .gp3p_stop("`x` must be a gp3bayes model card.")
  }
  x$evidence
}

#' Create a Reporting Checklist
#'
#' @param x A fitted `gp3bayes_fit` or `gp3bayes_model_card`.
#'
#' @return A data frame describing evidence that is available for reporting.
#' @export
create_reporting_checklist <- function(x) {
  card <- if (inherits(x, "gp3bayes_model_card")) {
    x
  } else {
    create_model_card(x)
  }

  diagnosis_available <- !inherits(card$diagnosis, "error")
  workflow_available <- !inherits(card$workflow, "error")

  data.frame(
    item = c(
      "model_family_recorded",
      "formula_recorded",
      "sampling_backend_recorded",
      "sampling_settings_recorded",
      "diagnostics_available",
      "workflow_status_available",
      "analysis_bundle_available",
      "analysis_manifest_available",
      "interpretation_boundary_recorded"
    ),
    available = c(
      !is.na(card$model_family) && nzchar(card$model_family),
      !is.na(card$formula) && nzchar(card$formula),
      !is.na(card$sampling_backend) && nzchar(card$sampling_backend),
      !is.null(card$sampling),
      diagnosis_available,
      workflow_available,
      !is.null(card$analysis_bundle),
      !is.null(card$manifest),
      nzchar(card$interpretation)
    ),
    automatic_requirement = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Plot a Reporting Checklist
#'
#' @param x A reporting checklist, fit, or model card.
#'
#' @return A `ggplot`.
#' @export
plot_reporting_checklist <- function(x) {
  .gp3g_require("ggplot2", "plot a reporting checklist")
  d <- if (is.data.frame(x)) x else create_reporting_checklist(x)
  if (!all(c("item", "available") %in% names(d))) {
    .gp3p_stop("`x` does not contain a reporting checklist.")
  }
  d$item <- factor(d$item, levels = rev(d$item))
  ggplot2::ggplot(d, ggplot2::aes(x = item, y = as.integer(available))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Not recorded", "Available")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "gp3bayes reporting evidence inventory"
    ) +
    theme_gp3bayes()
}

#' Write a Model Card
#'
#' @param x A `gp3bayes_model_card`.
#' @param file Explicit Markdown output path.
#' @param overwrite Whether an existing file may be replaced.
#'
#' @return Invisibly, the normalized written path.
#' @export
write_model_card <- function(x, file, overwrite = FALSE) {
  if (!inherits(x, "gp3bayes_model_card")) {
    .gp3p_stop("`x` must be a gp3bayes model card.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3p_stop("`file` must be one explicit output path.")
  }
  overwrite <- .gp3pr_flag(overwrite, "overwrite")
  if (file.exists(file) && !overwrite) {
    .gp3p_stop("`file` already exists; set `overwrite = TRUE` to replace it.")
  }
  parent <- dirname(file)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(parent)) .gp3p_stop("Could not create the output directory.")

  checklist <- create_reporting_checklist(x)
  lines <- c(
    "# gp3bayes model card",
    "",
    paste0("- Label: ", x$label %||% "not specified"),
    paste0("- Family: `", x$family, "`"),
    paste0("- Model family: `", x$model_family, "`"),
    paste0("- Formula: `", x$formula, "`"),
    paste0("- Sampling backend: `", x$sampling_backend, "`"),
    "",
    "## Evidence inventory",
    "",
    "```",
    utils::capture.output(print(x$evidence, row.names = FALSE)),
    "```",
    "",
    "## Reporting checklist",
    "",
    "```",
    utils::capture.output(print(checklist, row.names = FALSE)),
    "```",
    "",
    "## Interpretation boundary",
    "",
    x$interpretation,
    "",
    "- Model adequacy certified automatically: `FALSE`",
    "- Causal identification certified automatically: `FALSE`",
    "- Automatic model selection: `FALSE`",
    ""
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

# Publication registries, evidence inventories, and dashboards

.gp3pd_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3pd_name <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .gp3pd_stop("`", label, "` must be one non-empty character value.")
  }
  x
}

.gp3pd_registry <- function(x) {
  if (!inherits(x, "gp3bayes_publication_registry")) {
    .gp3pd_stop("`registry` must be a gp3bayes publication registry.")
  }
  invisible(x)
}

#' Create a Publication Registry
#'
#' @param label Optional registry label.
#' @return A `gp3bayes_publication_registry`.
#' @export
create_publication_registry <- function(label = NULL) {
  if (!is.null(label)) label <- .gp3pd_name(label, "label")
  structure(
    list(
      registry_version = "0.3",
      label = label,
      tables = list(),
      figures = list(),
      entries = data.frame(
        name = character(),
        type = character(),
        caption = character(),
        source = character(),
        stringsAsFactors = FALSE
      ),
      automatic_writing = FALSE
    ),
    class = "gp3bayes_publication_registry"
  )
}

#' Register a Publication Table
#'
#' @param registry A publication registry.
#' @param name Unique entry name.
#' @param table A data frame.
#' @param caption Optional caption.
#' @param source Optional source label.
#' @return An updated registry.
#' @export
register_publication_table <- function(
  registry,
  name,
  table,
  caption = NULL,
  source = NULL
) {
  .gp3pd_registry(registry)
  name <- .gp3pd_name(name, "name")
  if (!is.data.frame(table)) .gp3pd_stop("`table` must be a data frame.")
  if (name %in% registry$entries$name) .gp3pd_stop("Registry name already exists.")
  caption <- if (is.null(caption)) NA_character_ else .gp3pd_name(caption, "caption")
  source <- if (is.null(source)) NA_character_ else .gp3pd_name(source, "source")

  registry$tables[[name]] <- table
  registry$entries <- rbind(
    registry$entries,
    data.frame(
      name = name,
      type = "table",
      caption = caption,
      source = source,
      stringsAsFactors = FALSE
    )
  )
  registry
}

#' Register a Publication Figure
#'
#' @param registry A publication registry.
#' @param name Unique entry name.
#' @param figure A ggplot, bayesplot grid, or gtable.
#' @param caption Optional caption.
#' @param source Optional source label.
#' @return An updated registry.
#' @export
register_publication_figure <- function(
  registry,
  name,
  figure,
  caption = NULL,
  source = NULL
) {
  .gp3pd_registry(registry)
  name <- .gp3pd_name(name, "name")
  valid <- inherits(figure, "ggplot") ||
    inherits(figure, "bayesplot_grid") ||
    inherits(figure, "gtable")
  if (!valid) .gp3pd_stop("`figure` must be a supported plot object.")
  if (name %in% registry$entries$name) .gp3pd_stop("Registry name already exists.")
  caption <- if (is.null(caption)) NA_character_ else .gp3pd_name(caption, "caption")
  source <- if (is.null(source)) NA_character_ else .gp3pd_name(source, "source")

  registry$figures[[name]] <- figure
  registry$entries <- rbind(
    registry$entries,
    data.frame(
      name = name,
      type = "figure",
      caption = caption,
      source = source,
      stringsAsFactors = FALSE
    )
  )
  registry
}

#' Publication Registry Table
#'
#' @param x A publication registry.
#' @return Entry metadata.
#' @export
publication_registry_table <- function(x) {
  .gp3pd_registry(x)
  x$entries
}

#' Validate a Publication Registry
#'
#' @param x A publication registry.
#' @return A validation object.
#' @export
validate_publication_registry <- function(x) {
  .gp3pd_registry(x)
  issues <- character()
  if (anyDuplicated(x$entries$name)) issues <- c(issues, "duplicate names")
  if (!setequal(x$entries$name[x$entries$type == "table"], names(x$tables))) {
    issues <- c(issues, "table registry mismatch")
  }
  if (!setequal(x$entries$name[x$entries$type == "figure"], names(x$figures))) {
    issues <- c(issues, "figure registry mismatch")
  }
  if (length(x$tables) && !all(vapply(x$tables, is.data.frame, logical(1L)))) {
    issues <- c(issues, "invalid table entry")
  }

  structure(
    list(
      status = if (length(issues)) "fail" else "pass",
      valid = !length(issues),
      issues = issues,
      entries = nrow(x$entries),
      tables = length(x$tables),
      figures = length(x$figures),
      automatic_writing = FALSE
    ),
    class = "gp3bayes_publication_registry_validation"
  )
}

#' @export
print.gp3bayes_publication_registry <- function(x, ...) {
  cat("\ngp3bayes publication registry\n")
  if (!is.null(x$label)) cat(" Label: ", x$label, "\n", sep = "")
  cat(" Tables: ", length(x$tables), "\n", sep = "")
  cat(" Figures: ", length(x$figures), "\n", sep = "")
  cat(" Automatic writing: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_publication_registry_validation <- function(x, ...) {
  cat("\ngp3bayes publication-registry validation\n")
  cat(" Status: ", x$status, "\n", sep = "")
  if (length(x$issues)) cat(" Issues: ", paste(x$issues, collapse = "; "), "\n", sep = "")
  invisible(x)
}

#' Write a Publication Registry
#'
#' @param x A publication registry.
#' @param file Explicit Markdown output path.
#' @param overwrite Whether an existing file may be replaced.
#' @return Invisibly, the normalized output path.
#' @export
write_publication_registry <- function(x, file, overwrite = FALSE) {
  .gp3pd_registry(x)
  valid <- validate_publication_registry(x)
  if (!valid$valid) .gp3pd_stop("Registry validation failed.")
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3pd_stop("`file` must be one explicit path.")
  }
  overwrite <- .gp3pr_flag(overwrite, "overwrite")
  if (file.exists(file) && !overwrite) .gp3pd_stop("`file` already exists.")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# gp3bayes publication registry",
    "",
    paste0("- Label: ", x$label %||% "not specified"),
    paste0("- Tables: ", length(x$tables)),
    paste0("- Figures: ", length(x$figures)),
    "- Automatic writing: `FALSE`",
    "",
    "## Registry",
    "",
    "```",
    utils::capture.output(print(x$entries, row.names = FALSE)),
    "```",
    ""
  )
  for (nm in names(x$tables)) {
    lines <- c(
      lines,
      paste0("## Table: ", nm),
      "",
      "```",
      utils::capture.output(print(x$tables[[nm]], row.names = FALSE)),
      "```",
      ""
    )
  }
  if (length(x$figures)) {
    lines <- c(lines, "## Figures", "", paste0("- ", names(x$figures)), "")
  }
  writeLines(lines, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' Save Publication Registry Figures
#'
#' @param x A publication registry.
#' @param directory Explicit output directory.
#' @param width,height Figure dimensions in inches.
#' @param dpi Raster resolution.
#' @param device Graphics device/extension.
#' @param overwrite Whether existing files may be replaced.
#' @return Invisibly, written file paths.
#' @export
save_publication_registry_figures <- function(
  x,
  directory,
  width = 7,
  height = 5,
  dpi = 300,
  device = "png",
  overwrite = FALSE
) {
  .gp3pd_registry(x)
  if (!length(x$figures)) return(invisible(character()))
  fs <- structure(
    list(
      title = x$label %||% "gp3bayes publication figures",
      plots = x$figures,
      names = names(x$figures)
    ),
    class = "gp3bayes_figure_set"
  )
  save_figure_set(
    fs,
    directory = directory,
    width = width,
    height = height,
    dpi = dpi,
    device = device,
    overwrite = overwrite
  )
}

.gp3pd_status <- function(x) {
  if (is.null(x)) return("not_available")
  if (inherits(x, "error")) return("error")
  if (is.list(x) && !is.null(x$status) && length(x$status)) {
    return(as.character(x$status)[[1L]])
  }
  "available"
}

#' Create a Complete Evidence Inventory
#'
#' @param ... Named evidence objects.
#' @param label Optional label.
#' @return A `gp3bayes_evidence_inventory`.
#' @export
create_complete_evidence_inventory <- function(..., label = NULL) {
  objects <- list(...)
  if (!length(objects)) .gp3pd_stop("Supply at least one evidence object.")
  if (is.null(names(objects)) || any(!nzchar(names(objects))) ||
      anyDuplicated(names(objects))) {
    .gp3pd_stop("All evidence objects must have unique non-empty names.")
  }
  if (!is.null(label)) label <- .gp3pd_name(label, "label")

  table <- data.frame(
    component = names(objects),
    class = vapply(
      objects,
      function(z) paste(class(z), collapse = "/"),
      character(1L)
    ),
    status = vapply(objects, .gp3pd_status, character(1L)),
    available = !vapply(objects, is.null, logical(1L)),
    automatic_decision = FALSE,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      inventory_version = "0.3",
      label = label,
      objects = objects,
      table = table,
      automatic_decision = FALSE
    ),
    class = "gp3bayes_evidence_inventory"
  )
}

#' Evidence Inventory Table
#'
#' @param x A `gp3bayes_evidence_inventory`.
#' @return Inventory metadata.
#' @export
evidence_inventory_table <- function(x) {
  if (!inherits(x, "gp3bayes_evidence_inventory")) {
    .gp3pd_stop("`x` must be a gp3bayes evidence inventory.")
  }
  x$table
}

#' @export
print.gp3bayes_evidence_inventory <- function(x, ...) {
  cat("\ngp3bayes evidence inventory\n")
  cat(" Components: ", nrow(x$table), "\n", sep = "")
  cat(" Automatic decision: FALSE\n")
  invisible(x)
}

#' Create a Diagnostic Dashboard Object
#'
#' Expensive analyses are never launched implicitly. Supply already-computed
#' evidence objects.
#'
#' @param fit Optional fitted model.
#' @param analysis_bundle Optional analysis bundle.
#' @param model_card Optional model card.
#' @param loo Optional PSIS-LOO or LOO influence atlas.
#' @param prior_posterior Optional prior-posterior bridge.
#' @param sensitivity Optional sensitivity result.
#' @param recovery Optional recovery result.
#' @param sbc Optional SBC result.
#' @param label Optional label.
#' @return A `gp3bayes_diagnostic_dashboard`.
#' @export
create_diagnostic_dashboard <- function(
  fit = NULL,
  analysis_bundle = NULL,
  model_card = NULL,
  loo = NULL,
  prior_posterior = NULL,
  sensitivity = NULL,
  recovery = NULL,
  sbc = NULL,
  label = NULL
) {
  if (!is.null(fit)) .gp3p_validate_fit(fit)
  if (!is.null(analysis_bundle) &&
      !inherits(analysis_bundle, "gp3bayes_analysis_bundle")) {
    .gp3pd_stop("Unsupported `analysis_bundle`.")
  }
  if (!is.null(model_card) && !inherits(model_card, "gp3bayes_model_card")) {
    .gp3pd_stop("Unsupported `model_card`.")
  }
  if (!is.null(prior_posterior) &&
      !inherits(prior_posterior, "gp3bayes_prior_posterior_bridge")) {
    .gp3pd_stop("Unsupported `prior_posterior`.")
  }
  if (!is.null(recovery) && !inherits(recovery, "gp3bayes_recovery")) {
    .gp3pd_stop("Unsupported `recovery`.")
  }
  if (!is.null(sbc) && !inherits(sbc, "gp3bayes_sbc_result")) {
    .gp3pd_stop("Unsupported `sbc`.")
  }
  if (!is.null(label)) label <- .gp3pd_name(label, "label")

  objects <- list(
    fit = fit,
    analysis_bundle = analysis_bundle,
    model_card = model_card,
    loo = loo,
    prior_posterior = prior_posterior,
    sensitivity = sensitivity,
    recovery = recovery,
    sbc = sbc
  )
  if (all(vapply(objects, is.null, logical(1L)))) {
    .gp3pd_stop("Supply at least one evidence component.")
  }

  table <- data.frame(
    component = names(objects),
    available = !vapply(objects, is.null, logical(1L)),
    class = vapply(
      objects,
      function(z) if (is.null(z)) "" else paste(class(z), collapse = "/"),
      character(1L)
    ),
    status = vapply(objects, .gp3pd_status, character(1L)),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      dashboard_version = "0.3",
      label = label,
      objects = objects,
      table = table,
      automatic_decision = FALSE,
      interpretation = paste(
        "The dashboard indexes supplied evidence. It does not launch expensive",
        "analyses or issue an automatic adequacy verdict."
      )
    ),
    class = "gp3bayes_diagnostic_dashboard"
  )
}

#' Diagnostic Dashboard Table
#'
#' @param x A diagnostic dashboard.
#' @return Component availability and status.
#' @export
diagnostic_dashboard_table <- function(x) {
  if (!inherits(x, "gp3bayes_diagnostic_dashboard")) {
    .gp3pd_stop("`x` must be a gp3bayes diagnostic dashboard.")
  }
  x$table
}

#' @export
print.gp3bayes_diagnostic_dashboard <- function(x, ...) {
  cat("\ngp3bayes diagnostic dashboard\n")
  cat(" Available: ", sum(x$table$available), "/", nrow(x$table), "\n", sep = "")
  cat(" Automatic decision: FALSE\n")
  invisible(x)
}

#' Create Diagnostic Dashboard Figures
#'
#' @param x A diagnostic dashboard.
#' @return A `gp3bayes_figure_set` for supported evidence components.
#' @export
create_diagnostic_dashboard_figures <- function(x) {
  if (!inherits(x, "gp3bayes_diagnostic_dashboard")) {
    .gp3pd_stop("`x` must be a gp3bayes diagnostic dashboard.")
  }
  .gp3g_require("ggplot2", "create diagnostic dashboard figures")
  z <- x$objects
  plots <- list()

  if (!is.null(z$fit)) {
    plots$posterior_intervals <- plot_posterior_intervals(
      z$fit,
      regex = "^(b_|sd_|sigma$)"
    )
  }
  if (!is.null(z$model_card)) {
    plots$reporting_checklist <- plot_reporting_checklist(z$model_card)
  }
  if (!is.null(z$prior_posterior)) {
    plots$prior_posterior_shift <- plot_prior_posterior_shift(z$prior_posterior)
    plots$prior_posterior_contraction <- plot_prior_posterior_contraction(
      z$prior_posterior
    )
  }
  if (!is.null(z$loo)) {
    d <- if (inherits(z$loo, "gp3bayes_loo_influence_atlas")) {
      z$loo$table
    } else {
      loo_pointwise_table(z$loo)
    }
    plots$loo_influence <- plot_loo_influence_rank(d)
  }
  if (!is.null(z$recovery)) {
    plots$recovery_bias <- plot_recovery_bias(z$recovery)
    plots$recovery_coverage <- plot_recovery_coverage(z$recovery)
  }
  if (!is.null(z$sensitivity)) {
    if (inherits(z$sensitivity, "gp3bayes_prior_sensitivity")) {
      plots$sensitivity <- plot_prior_sensitivity(z$sensitivity)
    } else if (inherits(z$sensitivity, "gp3bayes_estimand_sensitivity")) {
      plots$sensitivity <- plot_estimand_sensitivity_gg(z$sensitivity)
    } else if (inherits(z$sensitivity, "gp3bayes_group_deletion_sensitivity")) {
      plots$sensitivity <- plot_group_deletion_sensitivity(z$sensitivity)
    } else if (inherits(z$sensitivity, "gp3bayes_random_slope_sensitivity")) {
      plots$sensitivity <- plot_random_slope_sensitivity(z$sensitivity)
    } else if (inherits(z$sensitivity, "gp3bayes_powerscale_sensitivity")) {
      plots$sensitivity <- plot_powerscale_sensitivity_gg(z$sensitivity)
    } else if (inherits(z$sensitivity, "gp3bayes_sensitivity_suite")) {
      plots$sensitivity <- plot_sensitivity_suite_gg(z$sensitivity)
    }
  }
  if (!is.null(z$sbc)) {
    plots$sbc_rank <- plot_sbc_rank_gg(z$sbc)
  }

  if (!length(plots)) .gp3pd_stop("No dashboard component can be plotted.")
  do.call(
    create_figure_set,
    c(plots, list(title = x$label %||% "gp3bayes diagnostic dashboard"))
  )
}

#' Plot Diagnostic Dashboard Availability
#'
#' @param x A diagnostic dashboard.
#' @return A `ggplot`.
#' @export
plot_diagnostic_dashboard <- function(x) {
  .gp3g_require("ggplot2", "plot dashboard availability")
  d <- diagnostic_dashboard_table(x)
  d$component <- factor(d$component, levels = rev(d$component))
  ggplot2::ggplot(d, ggplot2::aes(x = component, y = as.integer(available))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Unavailable", "Available")
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = x$label %||% "gp3bayes diagnostic dashboard"
    ) +
    theme_gp3bayes()
}

#' @export
plot.gp3bayes_diagnostic_dashboard <- function(x, ...) {
  plot_diagnostic_dashboard(x)
}

#' Write a Diagnostic Dashboard Report
#'
#' @param x A diagnostic dashboard.
#' @param file Explicit Markdown output path.
#' @param overwrite Whether an existing file may be replaced.
#' @return Invisibly, normalized output path.
#' @export
write_diagnostic_dashboard_report <- function(x, file, overwrite = FALSE) {
  if (!inherits(x, "gp3bayes_diagnostic_dashboard")) {
    .gp3pd_stop("`x` must be a gp3bayes diagnostic dashboard.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3pd_stop("`file` must be one explicit path.")
  }
  overwrite <- .gp3pr_flag(overwrite, "overwrite")
  if (file.exists(file) && !overwrite) .gp3pd_stop("`file` already exists.")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  lines <- c(
    "# gp3bayes diagnostic dashboard",
    "",
    paste0("- Label: ", x$label %||% "not specified"),
    paste0("- Available components: ", sum(x$table$available), "/", nrow(x$table)),
    "- Automatic decision: `FALSE`",
    "",
    "## Evidence availability",
    "",
    "```",
    utils::capture.output(print(x$table, row.names = FALSE)),
    "```",
    "",
    "## Interpretation boundary",
    "",
    x$interpretation,
    ""
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

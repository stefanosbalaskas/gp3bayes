# Unified workflow API for gp3bayes 0.2.0 stabilization

.gp3s_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3s_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3s_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3s_family <- function(x) {
  family <- x$family %||% NULL
  if (!is.null(family) && family %in% c("binary", "duration")) {
    return(family)
  }
  if (inherits(x, "gp3bayes_binary_fit") ||
      inherits(x, "gp3bayes_binary_model_specification") ||
      inherits(x, "gp3bayes_binary_prepared")) {
    return("binary")
  }
  if (inherits(x, "gp3bayes_duration_fit") ||
      inherits(x, "gp3bayes_duration_model_specification") ||
      inherits(x, "gp3bayes_duration_prepared")) {
    return("duration")
  }
  NA_character_
}

.gp3s_has_name <- function(x, name) {
  is.list(x) && name %in% names(x)
}

.gp3s_check_row <- function(check, status, detail) {
  data.frame(
    check = as.character(check),
    status = as.character(status),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

#' Validate a gp3bayes Object
#'
#' Performs lightweight structural validation for gp3bayes contracts,
#' prepared data, specifications, fits, summaries, diagnostics, manifests,
#' design audits, sensitivity suites, evidence collections, and backend
#' reliability objects. The function checks object structure only; it does not
#' establish statistical adequacy or substantive validity.
#'
#' @param x A gp3bayes object.
#' @param recursive Whether nested contract/specification/prepared objects
#'   should also be checked when present.
#' @param strict Whether a failed structural check should raise an error.
#'
#' @return A `gp3bayes_object_validation` object.
#' @export
validate_gp3bayes_object <- function(x, recursive = TRUE, strict = FALSE) {
  recursive <- .gp3s_flag(recursive, "recursive")
  strict <- .gp3s_flag(strict, "strict")

  classes <- class(x)
  recognized <- any(grepl("^gp3bayes_", classes))
  rows <- list(
    .gp3s_check_row(
      "gp3bayes_class",
      if (recognized) "pass" else "fail",
      paste(classes, collapse = ", ")
    )
  )

  family <- .gp3s_family(x)
  if (!is.na(family)) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "approved_family", "pass", family
    )
  }

  if (inherits(x, "gp3bayes_model_contract")) {
    required <- c(
      "contract_version", "family", "model_family", "mappings",
      "predictors", "random_slope", "likelihood", "link"
    )
    missing <- setdiff(required, names(x))
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "contract_fields",
      if (length(missing)) "fail" else "pass",
      if (length(missing)) paste(missing, collapse = ", ") else "complete"
    )
  }

  if (inherits(x, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    required <- c("data", "contract", "audit", "transformations")
    missing <- setdiff(required, names(x))
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "prepared_fields",
      if (length(missing)) "fail" else "pass",
      if (length(missing)) paste(missing, collapse = ", ") else "complete"
    )
    if (.gp3s_has_name(x, "data")) {
      rows[[length(rows) + 1L]] <- .gp3s_check_row(
        "prepared_data",
        if (is.data.frame(x$data) && nrow(x$data) > 0L) "pass" else "fail",
        if (is.data.frame(x$data)) paste0(nrow(x$data), " rows") else "not a data frame"
      )
    }
  }

  if (inherits(x, "gp3bayes_model_specification")) {
    required <- c("family", "contract", "prepared", "formula", "priors")
    missing <- setdiff(required, names(x))
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "specification_fields",
      if (length(missing)) "fail" else "pass",
      if (length(missing)) paste(missing, collapse = ", ") else "complete"
    )
  }

  if (inherits(x, "gp3bayes_fit")) {
    required <- c(
      "family", "specification", "backend_fit", "sampling_backend",
      "algorithm", "sampling", "fit_performed"
    )
    missing <- setdiff(required, names(x))
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "fit_fields",
      if (length(missing)) "fail" else "pass",
      if (length(missing)) paste(missing, collapse = ", ") else "complete"
    )
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "fit_performed",
      if (isTRUE(x$fit_performed)) "pass" else "review",
      as.character(isTRUE(x$fit_performed))
    )
  }

  if (inherits(x, "gp3bayes_sampling_diagnostics")) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "diagnostic_status",
      if (!is.null(x$status)) "pass" else "fail",
      x$status %||% "missing"
    )
  }

  if (inherits(x, c(
    "gp3bayes_binary_posterior_summary",
    "gp3bayes_duration_posterior_summary"
  ))) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "posterior_summary_table",
      if (is.data.frame(x$table) && nrow(x$table) > 0L) "pass" else "fail",
      if (is.data.frame(x$table)) paste0(nrow(x$table), " rows") else "missing"
    )
  }

  if (inherits(x, "gp3bayes_posterior_predictive_check")) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "ppc_status",
      if (!is.null(x$status)) "pass" else "fail",
      x$status %||% "missing"
    )
  }

  if (inherits(x, "gp3bayes_analysis_manifest")) {
    required <- c("manifest_version", "family", "data", "specification")
    missing <- setdiff(required, names(x))
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "manifest_fields",
      if (length(missing)) "fail" else "pass",
      if (length(missing)) paste(missing, collapse = ", ") else "complete"
    )
  }

  if (inherits(x, "gp3bayes_design_support_audit")) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "design_audit_status",
      if (!is.null(x$status)) "pass" else "fail",
      x$status %||% "missing"
    )
  }

  if (inherits(x, "gp3bayes_sensitivity_suite")) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "sensitivity_components",
      if (is.list(x$results)) "pass" else "fail",
      if (is.list(x$results)) paste(names(x$results), collapse = ", ") else "missing"
    )
  }

  if (inherits(x, "gp3bayes_model_evidence")) {
    rows[[length(rows) + 1L]] <- .gp3s_check_row(
      "evidence_components",
      if (is.list(x$components)) "pass" else "fail",
      if (is.list(x$components)) paste(names(x$components), collapse = ", ") else "missing"
    )
  }

  if (recursive && is.list(x)) {
    nested_names <- intersect(
      c("contract", "prepared", "specification"),
      names(x)
    )
    for (name in nested_names) {
      child <- x[[name]]
      if (is.null(child) || identical(child, x)) next
      if (any(grepl("^gp3bayes_", class(child)))) {
        child_validation <- validate_gp3bayes_object(
          child,
          recursive = FALSE,
          strict = FALSE
        )
        child_ok <- identical(child_validation$status, "pass")
        rows[[length(rows) + 1L]] <- .gp3s_check_row(
          paste0("nested_", name),
          if (child_ok) "pass" else "fail",
          child_validation$status
        )
      }
    }
  }

  table <- do.call(rbind, rows)
  status <- if (any(table$status == "fail")) {
    "fail"
  } else if (any(table$status == "review")) {
    "review"
  } else {
    "pass"
  }

  result <- structure(
    list(
      validation_version = "0.2",
      status = status,
      class = classes,
      family = family,
      checks = table,
      structural_validation_only = TRUE
    ),
    class = "gp3bayes_object_validation"
  )

  if (strict && identical(status, "fail")) {
    failed <- table$check[table$status == "fail"]
    .gp3s_stop(
      "gp3bayes object validation failed: ",
      paste(failed, collapse = ", "),
      "."
    )
  }

  result
}

#' @export
print.gp3bayes_object_validation <- function(x, ...) {
  cat("<gp3bayes_object_validation>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Class: ", paste(x$class, collapse = ", "), "\n", sep = "")
  if (!is.na(x$family)) cat("  Family: ", x$family, "\n", sep = "")
  counts <- table(factor(x$checks$status, levels = c("pass", "review", "fail")))
  cat(
    "  Checks: ", counts[["pass"]], " pass, ",
    counts[["review"]], " review, ", counts[["fail"]], " fail\n",
    sep = ""
  )
  invisible(x)
}

#' Diagnose an Approved gp3bayes Fit
#'
#' Family-neutral wrapper around [diagnose_binary_fit()] and
#' [diagnose_duration_fit()].
#'
#' @param fit A `gp3bayes_fit`.
#' @param ... Family-specific diagnostic arguments.
#' @return A family-specific `gp3bayes_sampling_diagnostics` object.
#' @export
diagnose_model_fit <- function(fit, ...) {
  validate_gp3bayes_object(fit, strict = TRUE)
  family <- .gp3s_family(fit)
  if (identical(family, "binary")) return(diagnose_binary_fit(fit, ...))
  if (identical(family, "duration")) return(diagnose_duration_fit(fit, ...))
  .gp3s_stop("Unsupported gp3bayes fit family.")
}

#' Summarise an Approved gp3bayes Posterior
#'
#' Family-neutral wrapper around [summarise_binary_posterior()] and
#' [summarise_duration_posterior()].
#'
#' @inheritParams diagnose_model_fit
#' @return A family-specific gp3bayes posterior summary.
#' @export
summarise_model_posterior <- function(fit, ...) {
  validate_gp3bayes_object(fit, strict = TRUE)
  family <- .gp3s_family(fit)
  if (identical(family, "binary")) return(summarise_binary_posterior(fit, ...))
  if (identical(family, "duration")) return(summarise_duration_posterior(fit, ...))
  .gp3s_stop("Unsupported gp3bayes fit family.")
}

#' Check Posterior Predictive Behaviour
#'
#' Family-neutral wrapper around the approved family-specific posterior
#' predictive checks. Passing this check is not a global adequacy claim.
#'
#' @inheritParams diagnose_model_fit
#' @return A family-specific `gp3bayes_posterior_predictive_check`.
#' @export
check_model_ppc <- function(fit, ...) {
  validate_gp3bayes_object(fit, strict = TRUE)
  family <- .gp3s_family(fit)
  if (identical(family, "binary")) {
    return(check_binary_posterior_predictive(fit, ...))
  }
  if (identical(family, "duration")) {
    return(check_duration_posterior_predictive(fit, ...))
  }
  .gp3s_stop("Unsupported gp3bayes fit family.")
}

#' Estimate the Approved Primary Estimands
#'
#' Dispatches to design-standardised probability contrasts for binary fits and
#' duration estimands for positive lognormal fits.
#'
#' @inheritParams diagnose_model_fit
#' @return A `gp3bayes_estimand`.
#' @export
estimate_model_estimands <- function(fit, ...) {
  validate_gp3bayes_object(fit, strict = TRUE)
  family <- .gp3s_family(fit)
  if (identical(family, "binary")) {
    return(estimate_standardized_probability_contrast(fit, ...))
  }
  if (identical(family, "duration")) {
    return(estimate_standardized_duration_estimands(fit, ...))
  }
  .gp3s_stop("Unsupported gp3bayes fit family.")
}

#' Summarise Workflow Stage Completion
#'
#' Creates an inspectable stage map for a gp3bayes object. Stage completion is
#' descriptive only and is not an adequacy or validity declaration.
#'
#' @param x A gp3bayes contract, prepared object, specification, fit, or
#'   evidence object.
#' @return A `gp3bayes_workflow_status` data frame.
#' @export
model_workflow_status <- function(x) {
  evidence_object <- inherits(x, "gp3bayes_model_evidence")
  source_fit <- if (inherits(x, "gp3bayes_fit")) {
    x
  } else if (evidence_object && inherits(x$fit, "gp3bayes_fit")) {
    x$fit
  } else {
    NULL
  }
  fitted <- !is.null(source_fit) || isTRUE(x$fit_performed)
  specification <- if (inherits(x, "gp3bayes_model_specification")) {
    x
  } else {
    x$specification %||% source_fit$specification %||% NULL
  }
  prepared <- if (inherits(x, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    x
  } else {
    specification$prepared %||% x$prepared %||% NULL
  }
  contract <- if (inherits(x, "gp3bayes_model_contract")) {
    x
  } else {
    specification$contract %||% prepared$contract %||% x$contract %||% NULL
  }

  evidence <- if (evidence_object) x$components else list()
  stages <- data.frame(
    stage = c(
      "contract", "prepared_data", "specification", "fit",
      "diagnostics", "posterior_summary", "ppc", "estimands",
      "sensitivity", "predictive_validation", "manifest"
    ),
    completed = c(
      !is.null(contract),
      !is.null(prepared),
      !is.null(specification),
      fitted,
      !is.null(evidence$diagnostics),
      !is.null(evidence$posterior),
      !is.null(evidence$ppc),
      !is.null(evidence$estimands),
      !is.null(evidence$sensitivity),
      !is.null(evidence$loo) || !is.null(evidence$kfold),
      !is.null(evidence$manifest) || inherits(x, "gp3bayes_analysis_manifest")
    ),
    stringsAsFactors = FALSE
  )
  structure(
    stages,
    class = c("gp3bayes_workflow_status", "data.frame")
  )
}

#' @export
print.gp3bayes_workflow_status <- function(x, ...) {
  cat("<gp3bayes_workflow_status>\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
plot.gp3bayes_workflow_status <- function(x, ...) {
  values <- as.integer(x$completed)
  graphics::barplot(
    values,
    names.arg = x$stage,
    las = 2,
    ylim = c(0, 1),
    ylab = "Completed (0/1)",
    main = "gp3bayes workflow stage map",
    ...
  )
  graphics::abline(h = 1, lty = 3)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_object_validation <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$checks, row.names = row.names, optional = optional, ...)
}

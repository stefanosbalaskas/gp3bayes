# Unified sensitivity and evidence objects for gp3bayes 0.2.0

.gp3e_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3e_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3e_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3e_status <- function(x) {
  if (is.null(x)) return("not_run")
  if (inherits(x, "error")) return("error")
  if (is.list(x) && !is.null(x$status)) return(as.character(x$status)[1L])
  if (isTRUE(x$adequacy_established)) return("pass")
  "completed"
}

.gp3e_safe <- function(expr, stop_on_error) {
  tryCatch(
    expr,
    error = function(e) {
      if (stop_on_error) stop(e)
      structure(
        list(status = "error", message = conditionMessage(e)),
        class = "gp3bayes_suite_error"
      )
    }
  )
}

.gp3e_call <- function(fun, args) {
  formal_names <- names(formals(fun))
  if (!"..." %in% formal_names) args <- args[names(args) %in% formal_names]
  do.call(fun, args)
}

#' Create a Sensitivity Suite Plan
#'
#' Creates a declarative plan for sensitivity analyses already supported by
#' gp3bayes. Expensive refits are opt-in and are never started merely by
#' creating a plan.
#'
#' @param prior_scale Whether to run the family-specific prior-scale refit.
#' @param powerscale Whether to run local power-scaling through `priorsense`.
#' @param psis_loo Whether to compute PSIS-LOO for the reference fit.
#' @param random_slope_plan Optional result of
#'   [create_random_slope_sensitivity_plan()].
#' @param group_deletion_plan Optional result of
#'   [create_group_deletion_sensitivity_plan()].
#' @param alternative_estimands Optional named list of already-computed
#'   estimands from coding/scaling/unit or other approved sensitivity fits.
#' @param duration_unit Optional list containing `estimand`, `multiplier`, and
#'   optional `tolerance` for [audit_duration_unit_invariance()].
#' @param prior_scale_args Named argument list passed to the
#'   family-specific prior-scale sensitivity function.
#' @param powerscale_args Named argument list passed to
#'   `assess_powerscaled_sensitivity()`.
#' @param psis_args Named argument list passed to `compute_psis_loo()`.
#' @param random_slope_args Named argument list passed to
#'   `run_random_slope_sensitivity()`.
#' @param group_deletion_args Named argument list passed to
#'   `run_group_deletion_sensitivity()`.
#' @return A `gp3bayes_sensitivity_plan`.
#' @export
create_sensitivity_suite_plan <- function(
  prior_scale = FALSE,
  powerscale = FALSE,
  psis_loo = FALSE,
  random_slope_plan = NULL,
  group_deletion_plan = NULL,
  alternative_estimands = list(),
  duration_unit = NULL,
  prior_scale_args = list(),
  powerscale_args = list(),
  psis_args = list(),
  random_slope_args = list(),
  group_deletion_args = list()
) {
  prior_scale <- .gp3e_flag(prior_scale, "prior_scale")
  powerscale <- .gp3e_flag(powerscale, "powerscale")
  psis_loo <- .gp3e_flag(psis_loo, "psis_loo")
  list_args <- list(
    prior_scale_args = prior_scale_args,
    powerscale_args = powerscale_args,
    psis_args = psis_args,
    random_slope_args = random_slope_args,
    group_deletion_args = group_deletion_args,
    alternative_estimands = alternative_estimands
  )
  invalid <- names(list_args)[!vapply(list_args, is.list, logical(1L))]
  if (length(invalid)) {
    .gp3e_stop("These arguments must be lists: ", paste(invalid, collapse = ", "), ".")
  }
  if (!is.null(random_slope_plan) &&
      !inherits(random_slope_plan, "gp3bayes_random_slope_sensitivity_plan")) {
    .gp3e_stop("`random_slope_plan` has an unsupported class.")
  }
  if (!is.null(group_deletion_plan) &&
      !inherits(group_deletion_plan, "gp3bayes_group_deletion_sensitivity_plan")) {
    .gp3e_stop("`group_deletion_plan` has an unsupported class.")
  }
  structure(
    list(
      plan_version = "0.2",
      prior_scale = list(run = prior_scale, args = prior_scale_args),
      powerscale = list(run = powerscale, args = powerscale_args),
      psis_loo = list(run = psis_loo, args = psis_args),
      random_slope = list(plan = random_slope_plan, args = random_slope_args),
      group_deletion = list(plan = group_deletion_plan, args = group_deletion_args),
      alternative_estimands = alternative_estimands,
      duration_unit = duration_unit,
      automatic_model_selection = FALSE,
      automatic_exclusion = FALSE
    ),
    class = "gp3bayes_sensitivity_plan"
  )
}

#' Run a Unified Sensitivity Suite
#'
#' Orchestrates approved sensitivity components. Refitting components only run
#' when explicitly requested in `plan`. Failures are retained as inspectable
#' component results unless `stop_on_error = TRUE`.
#'
#' @param fit An approved gp3bayes fit.
#' @param plan A [create_sensitivity_suite_plan()] result.
#' @param reference_estimand Optional precomputed primary estimand. When
#'   alternatives are supplied and this is omitted it is computed with
#'   [estimate_model_estimands()].
#' @param stop_on_error Whether the first component error should stop the suite.
#' @return A `gp3bayes_sensitivity_suite`.
#' @export
run_sensitivity_suite <- function(
  fit,
  plan = create_sensitivity_suite_plan(),
  reference_estimand = NULL,
  stop_on_error = FALSE
) {
  if (!inherits(fit, "gp3bayes_fit")) .gp3e_stop("`fit` must inherit from `gp3bayes_fit`.")
  if (!inherits(plan, "gp3bayes_sensitivity_plan")) {
    .gp3e_stop("`plan` must be created by `create_sensitivity_suite_plan()`.")
  }
  stop_on_error <- .gp3e_flag(stop_on_error, "stop_on_error")
  results <- list()

  if (isTRUE(plan$prior_scale$run)) {
    fun <- if (identical(fit$family, "binary")) {
      assess_binary_prior_sensitivity
    } else {
      assess_duration_prior_sensitivity
    }
    args <- c(list(fit = fit), plan$prior_scale$args)
    results$prior_scale <- .gp3e_safe(.gp3e_call(fun, args), stop_on_error)
  }

  if (isTRUE(plan$powerscale$run)) {
    args <- c(list(fit = fit), plan$powerscale$args)
    results$powerscale <- .gp3e_safe(
      .gp3e_call(assess_powerscaled_sensitivity, args),
      stop_on_error
    )
  }

  if (isTRUE(plan$psis_loo$run)) {
    args <- c(list(fit = fit), plan$psis_loo$args)
    results$psis_loo <- .gp3e_safe(
      .gp3e_call(compute_psis_loo, args),
      stop_on_error
    )
  }

  if (!is.null(plan$random_slope$plan)) {
    args <- c(list(plan = plan$random_slope$plan), plan$random_slope$args)
    results$random_slope <- .gp3e_safe(
      .gp3e_call(run_random_slope_sensitivity, args),
      stop_on_error
    )
  }

  if (!is.null(plan$group_deletion$plan)) {
    args <- c(list(plan = plan$group_deletion$plan), plan$group_deletion$args)
    results$group_deletion <- .gp3e_safe(
      .gp3e_call(run_group_deletion_sensitivity, args),
      stop_on_error
    )
  }

  if (length(plan$alternative_estimands)) {
    if (is.null(reference_estimand)) {
      reference_estimand <- .gp3e_safe(estimate_model_estimands(fit), stop_on_error)
    }
    if (!inherits(reference_estimand, "gp3bayes_suite_error")) {
      results$estimand_alternatives <- .gp3e_safe(
        compare_estimand_sensitivity(
          reference_estimand,
          plan$alternative_estimands
        ),
        stop_on_error
      )
    }
  }

  if (!is.null(plan$duration_unit)) {
    du <- plan$duration_unit
    required <- c("estimand", "multiplier")
    if (!all(required %in% names(du))) {
      .gp3e_stop("`duration_unit` must contain `estimand` and `multiplier`.")
    }
    if (is.null(reference_estimand)) {
      reference_estimand <- .gp3e_safe(estimate_model_estimands(fit), stop_on_error)
    }
    args <- list(
      reference = reference_estimand,
      converted = du$estimand,
      multiplier = du$multiplier,
      tolerance = du$tolerance %||% 0.02
    )
    results$duration_unit <- .gp3e_safe(
      do.call(audit_duration_unit_invariance, args),
      stop_on_error
    )
  }

  statuses <- if (length(results)) {
    vapply(results, .gp3e_status, character(1L))
  } else {
    character()
  }
  overall <- if (any(statuses %in% c("error", "fail"))) {
    "review"
  } else if (any(statuses %in% c("review", "warn", "not_assessed"))) {
    "review"
  } else if (length(statuses)) {
    "completed"
  } else {
    "not_run"
  }
  structure(
    list(
      suite_version = "0.2",
      family = fit$family,
      status = overall,
      fit = fit,
      plan = plan,
      reference_estimand = reference_estimand,
      results = results,
      component_status = data.frame(
        component = names(statuses),
        status = unname(statuses),
        stringsAsFactors = FALSE
      ),
      robustness_established = FALSE,
      automatic_model_selection = FALSE,
      automatic_exclusion = FALSE,
      interpretation = paste(
        "The suite collates declared sensitivity analyses.",
        "It does not convert component stability into a universal robustness claim."
      )
    ),
    class = "gp3bayes_sensitivity_suite"
  )
}

#' Summarise a Sensitivity Suite
#'
#' @param x A `gp3bayes_sensitivity_suite`.
#' @return A component-level data frame.
#' @export
summarise_sensitivity_suite <- function(x) {
  if (!inherits(x, "gp3bayes_sensitivity_suite")) {
    .gp3e_stop("`x` must be a gp3bayes sensitivity suite.")
  }
  table <- x$component_status
  if (!nrow(table)) {
    return(data.frame(
      component = character(), status = character(), detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  table$detail <- vapply(table$component, function(name) {
    result <- x$results[[name]]
    if (inherits(result, "gp3bayes_suite_error")) return(result$message)
    interpretation <- result$interpretation %||% NULL
    if (!is.null(interpretation)) return(paste(interpretation, collapse = " "))
    paste(class(result), collapse = ", ")
  }, character(1L))
  table
}

.gp3e_component_status <- function(x) {
  if (is.null(x)) return("not_supplied")
  if (inherits(x, "error") || inherits(x, "gp3bayes_suite_error")) return("error")
  x$status %||% "available"
}

#' Collect Model Evidence Without Declaring Model Adequacy
#'
#' Collects already-computed design, diagnostics, posterior summaries,
#' posterior predictive checks, estimands, predictive validation, sensitivity,
#' and reproducibility provenance into a single review object.
#'
#' @param fit Optional gp3bayes fit.
#' @param design,diagnostics,posterior,ppc,estimands,loo,kfold,sensitivity
#'   Optional evidence components.
#' @param manifest Optional `gp3bayes_analysis_manifest` containing
#'   reproducibility provenance for the analysis.
#' @param compute Character vector selecting inexpensive components to compute
#'   from `fit` when not supplied. Supported values are `"diagnostics"`,
#'   `"posterior"`, and `"estimands"`. Posterior predictive checks and
#'   refitting sensitivity are intentionally not automatic.
#' @return A `gp3bayes_model_evidence`.
#' @export
collect_model_evidence <- function(
  fit = NULL,
  design = NULL,
  diagnostics = NULL,
  posterior = NULL,
  ppc = NULL,
  estimands = NULL,
  loo = NULL,
  kfold = NULL,
  sensitivity = NULL,
  manifest = NULL,
  compute = character()
) {
  allowed <- c("diagnostics", "posterior", "estimands")
  if (!is.character(compute) || anyNA(compute) || any(!compute %in% allowed)) {
    .gp3e_stop("`compute` may contain only: ", paste(allowed, collapse = ", "), ".")
  }
  compute <- unique(compute)
  if (length(compute) && (is.null(fit) || !inherits(fit, "gp3bayes_fit"))) {
    .gp3e_stop("A gp3bayes `fit` is required for requested computed components.")
  }
  if ("diagnostics" %in% compute && is.null(diagnostics)) {
    diagnostics <- diagnose_model_fit(fit)
  }
  if ("posterior" %in% compute && is.null(posterior)) {
    posterior <- summarise_model_posterior(fit)
  }
  if ("estimands" %in% compute && is.null(estimands)) {
    estimands <- estimate_model_estimands(fit)
  }

  components <- list(
    design = design,
    diagnostics = diagnostics,
    posterior = posterior,
    ppc = ppc,
    estimands = estimands,
    loo = loo,
    kfold = kfold,
    sensitivity = sensitivity,
    manifest = manifest
  )
  statuses <- vapply(components, .gp3e_component_status, character(1L))
  table <- data.frame(
    component = names(components),
    available = !vapply(components, is.null, logical(1L)),
    status = unname(statuses),
    stringsAsFactors = FALSE
  )
  family <- if (!is.null(fit)) fit$family else {
    manifest$family %||% estimands$family %||% NA_character_
  }
  structure(
    list(
      evidence_version = "0.2",
      family = family,
      fit = fit,
      components = components,
      component_table = table,
      adequacy_established = FALSE,
      robustness_established = FALSE,
      causal_identification_established = FALSE,
      automatic_model_selection = FALSE,
      interpretation = paste(
        "Evidence components are collected for transparent review.",
        "No aggregate pass/fail adequacy verdict is generated."
      )
    ),
    class = "gp3bayes_model_evidence"
  )
}

#' Create a Model Evidence Report
#'
#' Writes an explicit Markdown inventory of available evidence components.
#'
#' @param evidence A `gp3bayes_model_evidence` object.
#' @param file Explicit Markdown output path.
#' @param overwrite Whether an existing file may be replaced.
#' @return The normalized output path, invisibly.
#' @export
create_model_evidence_report <- function(evidence, file, overwrite = FALSE) {
  if (!inherits(evidence, "gp3bayes_model_evidence")) {
    .gp3e_stop("`evidence` must be created by `collect_model_evidence()`.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3e_stop("`file` must be one explicit non-empty path.")
  }
  overwrite <- .gp3e_flag(overwrite, "overwrite")
  if (file.exists(file) && !overwrite) {
    .gp3e_stop("`file` already exists. Set `overwrite = TRUE` to replace it.")
  }
  if (!dir.exists(dirname(file))) .gp3e_stop("The report parent directory does not exist.")
  table <- evidence$component_table
  component_lines <- paste0(
    "- ", table$component, ": ",
    ifelse(table$available, paste0("available (", table$status, ")"), "not supplied")
  )
  lines <- c(
    "# gp3bayes model evidence report",
    "",
    paste0("Family: ", evidence$family),
    "",
    "## Evidence inventory",
    "",
    component_lines,
    "",
    "## Interpretation boundary",
    "",
    paste(
      "This report is an evidence inventory. It does not automatically establish",
      "convergence, posterior adequacy, robustness, causal identification,",
      "substantive validity, or a preferred model."
    )
  )
  writeLines(lines, con = file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' @export
print.gp3bayes_sensitivity_plan <- function(x, ...) {
  cat("<gp3bayes_sensitivity_plan>\n")
  cat("  Prior-scale refit: ", x$prior_scale$run, "\n", sep = "")
  cat("  Power-scaling: ", x$powerscale$run, "\n", sep = "")
  cat("  PSIS-LOO: ", x$psis_loo$run, "\n", sep = "")
  cat("  Random-slope plan: ", !is.null(x$random_slope$plan), "\n", sep = "")
  cat("  Group-deletion plan: ", !is.null(x$group_deletion$plan), "\n", sep = "")
  invisible(x)
}

#' @export
print.gp3bayes_sensitivity_suite <- function(x, ...) {
  cat("<gp3bayes_sensitivity_suite>\n")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Status: ", x$status, "\n", sep = "")
  if (nrow(x$component_status)) print.data.frame(x$component_status, row.names = FALSE)
  cat("  Universal robustness established: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_model_evidence <- function(x, ...) {
  cat("<gp3bayes_model_evidence>\n")
  if (!is.na(x$family)) cat("  Family: ", x$family, "\n", sep = "")
  print.data.frame(x$component_table, row.names = FALSE)
  cat("  Automatic adequacy verdict: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_sensitivity_suite <- function(x, ...) {
  if (!nrow(x$component_status)) return(invisible(x))
  categories <- c("completed", "pass", "review", "fail", "error", "not_assessed")
  code <- match(x$component_status$status, categories)
  code[is.na(code)] <- 1L
  graphics::barplot(
    code,
    names.arg = x$component_status$component,
    las = 2,
    ylab = "Component status code",
    main = "Sensitivity suite components",
    ...
  )
  graphics::axis(2, at = seq_along(categories), labels = categories, las = 2)
  invisible(x)
}

#' @export
plot.gp3bayes_model_evidence <- function(x, ...) {
  values <- as.integer(x$component_table$available)
  graphics::barplot(
    values,
    names.arg = x$component_table$component,
    las = 2,
    ylim = c(0, 1),
    ylab = "Available (0/1)",
    main = "Model evidence inventory",
    ...
  )
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_sensitivity_suite <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(
    summarise_sensitivity_suite(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' @export
as.data.frame.gp3bayes_model_evidence <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$component_table, row.names = row.names, optional = optional, ...)
}

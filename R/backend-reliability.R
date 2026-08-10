# Backend reliability and object-schema contracts for gp3bayes 0.2.0

.gp3br_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3br_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3br_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3br_number <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > upper) {
    .gp3br_stop(
      "`", name, "` must be one finite numeric value in [",
      lower, ", ", upper, "]."
    )
  }
  as.numeric(x)
}

.gp3br_character <- function(x, name, optional = FALSE) {
  if (optional && is.null(x)) return(NULL)
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    .gp3br_stop("`", name, "` must contain non-empty character values.")
  }
  unique(x)
}

.gp3br_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}

.gp3br_backend_from_fit <- function(fit) {
  if (!inherits(fit, "gp3bayes_fit")) return(NA_character_)
  backend <- fit$sampling_backend %||% NA_character_
  if (!backend %in% c("rstan", "cmdstanr")) NA_character_ else backend
}

#' Report Bayesian Backend Capabilities
#'
#' Provides a stable 0.2.0-facing capability table for the two approved Stan
#' backends. It augments the existing package capability report with package
#' versions, CmdStan installation information, and explicit readiness fields.
#' No model is compiled or fitted.
#'
#' @return A `gp3bayes_backend_capabilities_v2` data frame.
#' @examples
#' backend_capabilities()
#' @export
backend_capabilities <- function() {
  base <- bayesian_backend_capabilities()

  rstan_available <- requireNamespace("rstan", quietly = TRUE)
  cmdstanr_available <- requireNamespace("cmdstanr", quietly = TRUE)
  brms_available <- requireNamespace("brms", quietly = TRUE)

  cmdstan_version <- NA_character_
  cmdstan_path <- NA_character_
  cmdstan_ready <- FALSE

  if (cmdstanr_available) {
    cmdstan_version <- tryCatch(
      {
        value <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
        if (is.null(value)) NA_character_ else as.character(value)
      },
      error = function(error) NA_character_
    )
    cmdstan_path <- tryCatch(
      cmdstanr::cmdstan_path(),
      error = function(error) NA_character_
    )
    cmdstan_ready <- !is.na(cmdstan_version) && !is.na(cmdstan_path)
  }

  result <- data.frame(
    backend = c("rstan", "cmdstanr"),
    brms_available = rep(brms_available, 2L),
    backend_package_available = c(rstan_available, cmdstanr_available),
    backend_package_version = c(
      .gp3br_version("rstan"),
      .gp3br_version("cmdstanr")
    ),
    external_runtime_available = c(TRUE, cmdstan_ready),
    external_runtime_version = c(NA_character_, cmdstan_version),
    ready_for_package_interface = c(
      brms_available && rstan_available,
      brms_available && cmdstanr_available && cmdstan_ready
    ),
    algorithm = rep("sampling", 2L),
    model_family_scope = rep(
      "Bernoulli-logit and positive uncensored lognormal duration",
      2L
    ),
    unrestricted_modeling = rep(FALSE, 2L),
    stringsAsFactors = FALSE
  )

  attr(result, "legacy_capabilities") <- base
  attr(result, "cmdstan_path") <- cmdstan_path

  structure(
    result,
    class = c("gp3bayes_backend_capabilities_v2", class(result))
  )
}

#' @export
print.gp3bayes_backend_capabilities_v2 <- function(x, ...) {
  cat("<gp3bayes_backend_capabilities_v2>\n")
  print.data.frame(as.data.frame(x), row.names = FALSE)
  invisible(x)
}

#' Validate a Bayesian Backend Environment
#'
#' Checks whether the packages and external runtime required by one approved
#' gp3bayes backend are available. With `compile_test = TRUE`, an optional
#' minimal compiler smoke test is performed. The smoke test does not fit a
#' statistical model and writes no persistent files.
#'
#' @param backend Either `"rstan"` or `"cmdstanr"`.
#' @param compile_test Whether to run an optional compiler smoke test.
#' @param strict Whether an unavailable backend should raise an error.
#'
#' @return A `gp3bayes_backend_environment` object.
#' @examples
#' validate_backend_environment("rstan")
#' validate_backend_environment("cmdstanr")
#' @export
validate_backend_environment <- function(
  backend = c("rstan", "cmdstanr"),
  compile_test = FALSE,
  strict = FALSE
) {
  backend <- match.arg(backend)
  compile_test <- .gp3br_flag(compile_test, "compile_test")
  strict <- .gp3br_flag(strict, "strict")

  capabilities <- backend_capabilities()
  row <- capabilities[capabilities$backend == backend, , drop = FALSE]

  checks <- data.frame(
    check = c("brms_package", "backend_package", "external_runtime"),
    status = c(
      if (isTRUE(row$brms_available)) "pass" else "fail",
      if (isTRUE(row$backend_package_available)) "pass" else "fail",
      if (isTRUE(row$external_runtime_available)) "pass" else "fail"
    ),
    detail = c(
      .gp3br_version("brms"),
      row$backend_package_version,
      if (backend == "cmdstanr") {
        row$external_runtime_version
      } else {
        "managed by rstan package/toolchain"
      }
    ),
    stringsAsFactors = FALSE
  )

  compile_detail <- "not requested"
  compile_status <- "not_assessed"

  if (compile_test) {
    if (any(checks$status == "fail")) {
      compile_status <- "not_assessed"
      compile_detail <- "prerequisite check failed"
    } else if (backend == "cmdstanr") {
      toolchain <- tryCatch(
        {
          cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = TRUE)
          TRUE
        },
        error = function(error) error
      )
      if (isTRUE(toolchain)) {
        compile_status <- "pass"
        compile_detail <- "CmdStan toolchain check passed"
      } else {
        compile_status <- "fail"
        compile_detail <- conditionMessage(toolchain)
      }
    } else {
      compiled <- tryCatch(
        {
          rstan::stan_model(
            model_code = paste(
              "parameters { real y; }",
              "model { y ~ normal(0, 1); }"
            ),
            auto_write = FALSE,
            verbose = FALSE
          )
          TRUE
        },
        error = function(error) error
      )
      if (isTRUE(compiled)) {
        compile_status <- "pass"
        compile_detail <- "minimal rstan compilation passed"
      } else {
        compile_status <- "fail"
        compile_detail <- conditionMessage(compiled)
      }
    }
  }

  checks <- rbind(
    checks,
    data.frame(
      check = "compiler_smoke_test",
      status = compile_status,
      detail = compile_detail,
      stringsAsFactors = FALSE
    )
  )

  status <- if (any(checks$status == "fail")) {
    "fail"
  } else if (any(checks$status == "not_assessed")) {
    "ready"
  } else {
    "pass"
  }

  result <- structure(
    list(
      validation_version = "0.2",
      backend = backend,
      status = status,
      checks = checks,
      capabilities = row,
      compile_test = compile_test,
      model_fitted = FALSE
    ),
    class = "gp3bayes_backend_environment"
  )

  if (strict && identical(status, "fail")) {
    failed <- checks$check[checks$status == "fail"]
    .gp3br_stop(
      "Backend environment validation failed for `", backend, "`: ",
      paste(failed, collapse = ", "), "."
    )
  }

  result
}

#' @export
print.gp3bayes_backend_environment <- function(x, ...) {
  cat("<gp3bayes_backend_environment>\n")
  cat("  Backend: ", x$backend, "\n", sep = "")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Compiler smoke test requested: ", x$compile_test, "\n", sep = "")
  print(x$checks, row.names = FALSE)
  invisible(x)
}

#' @export
plot.gp3bayes_backend_environment <- function(x, ...) {
  levels <- c("pass", "ready", "not_assessed", "fail")
  values <- match(x$checks$status, levels)
  values[is.na(values)] <- match("not_assessed", levels)
  graphics::barplot(
    values,
    names.arg = x$checks$check,
    las = 2,
    ylim = c(0, length(levels) + 0.5),
    ylab = "Environment status",
    main = paste("Backend environment:", x$backend),
    ...
  )
  graphics::axis(2, at = seq_along(levels), labels = levels, las = 1)
  invisible(x)
}

.gp3br_draw_summary <- function(fit, variables = NULL) {
  if (!inherits(fit, "gp3bayes_fit")) {
    .gp3br_stop("Backend parity inputs must inherit from `gp3bayes_fit`.")
  }
  if (!requireNamespace("posterior", quietly = TRUE)) {
    .gp3br_stop("Package `posterior` is required for backend parity auditing.")
  }
  if (is.null(fit$backend_fit)) {
    .gp3br_stop("The fit does not contain `backend_fit` posterior draws.")
  }

  if (is.null(variables)) {
    variables <- .gp3b_parameter_variables(fit$backend_fit, include = NULL)
  } else {
    variables <- .gp3br_character(variables, "variables")
  }
  draws <- posterior::as_draws_array(
    fit$backend_fit,
    variable = variables
  )

  summary <- posterior::summarise_draws(
    draws,
    mean = mean,
    median = stats::median,
    sd = stats::sd,
    mcse_mean = posterior::mcse_mean,
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )

  as.data.frame(summary, stringsAsFactors = FALSE)
}

.gp3br_summary_input <- function(x, variables = NULL) {
  if (inherits(x, "gp3bayes_fit")) {
    return(.gp3br_draw_summary(x, variables = variables))
  }
  if (!is.data.frame(x)) {
    .gp3br_stop(
      "Parity inputs must be gp3bayes fits or posterior summary data frames."
    )
  }
  required <- c("variable", "mean", "sd", "mcse_mean")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    .gp3br_stop(
      "Parity summary data frame is missing: ",
      paste(missing, collapse = ", "), "."
    )
  }
  if (!is.null(variables)) {
    variables <- .gp3br_character(variables, "variables")
    x <- x[x$variable %in% variables, , drop = FALSE]
  }
  x
}

#' Audit Posterior Parity Across rstan and cmdstanr
#'
#' Compares posterior summaries from two independently sampled fits. Mean
#' differences are evaluated relative to the combined Monte Carlo standard
#' error rather than requiring identical draws. Standard-deviation differences
#' are reported separately. A parity pass is a computational consistency check,
#' not evidence that either model is statistically or substantively adequate.
#'
#' @param rstan_fit A gp3bayes rstan fit, or a compatible posterior-summary
#'   data frame for testing/auditing.
#' @param cmdstanr_fit A gp3bayes cmdstanr fit, or a compatible summary table.
#' @param variables Optional parameter names to compare. When omitted, the
#'   package's approved population/group-scale parameter set is used.
#' @param mcse_multiplier Multiplier applied to the combined MCSE of posterior
#'   means to define a sampling-noise comparison band.
#' @param absolute_tolerance Minimum absolute tolerance for mean differences.
#' @param relative_sd_tolerance Review threshold for relative posterior-SD
#'   differences.
#'
#' @return A `gp3bayes_backend_parity_audit`.
#' @examples
#' rstan_summary <- data.frame(
#'   variable = c("b_Intercept", "b_conditiontreatment"),
#'   mean = c(-0.6, 0.4), sd = c(0.20, 0.15),
#'   mcse_mean = c(0.01, 0.01)
#' )
#' cmdstanr_summary <- data.frame(
#'   variable = c("b_Intercept", "b_conditiontreatment"),
#'   mean = c(-0.59, 0.41), sd = c(0.21, 0.15),
#'   mcse_mean = c(0.01, 0.01)
#' )
#' audit_backend_parity(rstan_summary, cmdstanr_summary)
#' @export
audit_backend_parity <- function(
  rstan_fit,
  cmdstanr_fit,
  variables = NULL,
  mcse_multiplier = 3,
  absolute_tolerance = 0,
  relative_sd_tolerance = 0.10
) {
  mcse_multiplier <- .gp3br_number(
    mcse_multiplier, "mcse_multiplier", lower = 0
  )
  absolute_tolerance <- .gp3br_number(
    absolute_tolerance, "absolute_tolerance", lower = 0
  )
  relative_sd_tolerance <- .gp3br_number(
    relative_sd_tolerance, "relative_sd_tolerance", lower = 0
  )

  if (inherits(rstan_fit, "gp3bayes_fit")) {
    backend <- .gp3br_backend_from_fit(rstan_fit)
    if (!identical(backend, "rstan")) {
      .gp3br_stop("`rstan_fit` must record `sampling_backend = \"rstan\"`.")
    }
  }
  if (inherits(cmdstanr_fit, "gp3bayes_fit")) {
    backend <- .gp3br_backend_from_fit(cmdstanr_fit)
    if (!identical(backend, "cmdstanr")) {
      .gp3br_stop("`cmdstanr_fit` must record `sampling_backend = \"cmdstanr\"`.")
    }
  }
  if (inherits(rstan_fit, "gp3bayes_fit") &&
      inherits(cmdstanr_fit, "gp3bayes_fit") &&
      !identical(rstan_fit$family, cmdstanr_fit$family)) {
    .gp3br_stop("Backend parity fits must use the same gp3bayes family.")
  }

  left <- .gp3br_summary_input(rstan_fit, variables = variables)
  right <- .gp3br_summary_input(cmdstanr_fit, variables = variables)

  left_variables <- unique(left$variable)
  right_variables <- unique(right$variable)
  missing_rstan <- setdiff(right_variables, left_variables)
  missing_cmdstanr <- setdiff(left_variables, right_variables)

  common <- intersect(left_variables, right_variables)
  if (!length(common)) {
    .gp3br_stop("No common posterior variables were available for comparison.")
  }

  left <- left[match(common, left$variable), , drop = FALSE]
  right <- right[match(common, right$variable), , drop = FALSE]

  combined_mcse <- sqrt(left$mcse_mean^2 + right$mcse_mean^2)
  mean_difference <- left$mean - right$mean
  allowed_mean_difference <- pmax(
    absolute_tolerance,
    mcse_multiplier * combined_mcse
  )
  mean_finite <- is.finite(left$mean) & is.finite(right$mean) &
    is.finite(combined_mcse) & is.finite(allowed_mean_difference)
  mean_within_mcse <- mean_finite &
    abs(mean_difference) <= allowed_mean_difference

  sd_scale <- pmax(abs(left$sd), abs(right$sd), .Machine$double.eps)
  relative_sd_difference <- abs(left$sd - right$sd) / sd_scale
  sd_finite <- is.finite(left$sd) & is.finite(right$sd) &
    is.finite(relative_sd_difference)
  sd_within_tolerance <- sd_finite &
    relative_sd_difference <= relative_sd_tolerance

  table <- data.frame(
    variable = common,
    rstan_mean = left$mean,
    cmdstanr_mean = right$mean,
    mean_difference = mean_difference,
    combined_mcse = combined_mcse,
    allowed_mean_difference = allowed_mean_difference,
    mean_within_mcse = mean_within_mcse,
    rstan_sd = left$sd,
    cmdstanr_sd = right$sd,
    relative_sd_difference = relative_sd_difference,
    sd_within_tolerance = sd_within_tolerance,
    stringsAsFactors = FALSE
  )
  table$status <- ifelse(
    table$mean_within_mcse & table$sd_within_tolerance,
    "pass",
    "review"
  )

  status <- if (all(table$status == "pass") &&
                !length(missing_rstan) && !length(missing_cmdstanr)) {
    "pass"
  } else {
    "review"
  }

  structure(
    list(
      parity_version = "0.2",
      status = status,
      table = table,
      missing_from_rstan = missing_rstan,
      missing_from_cmdstanr = missing_cmdstanr,
      settings = list(
        mcse_multiplier = mcse_multiplier,
        absolute_tolerance = absolute_tolerance,
        relative_sd_tolerance = relative_sd_tolerance
      ),
      identical_draws_expected = FALSE,
      model_adequacy_established = FALSE,
      interpretation = paste(
        "Posterior summaries are compared relative to Monte Carlo uncertainty",
        "and posterior scale. A pass supports backend consistency for the",
        "quantities assessed; it does not establish convergence, model adequacy,",
        "substantive validity, or causal identification."
      )
    ),
    class = "gp3bayes_backend_parity_audit"
  )
}

#' @export
print.gp3bayes_backend_parity_audit <- function(x, ...) {
  cat("<gp3bayes_backend_parity_audit>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Parameters compared: ", nrow(x$table), "\n", sep = "")
  cat("  Review parameters: ", sum(x$table$status == "review"), "\n", sep = "")
  cat("  Identical draws expected: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_backend_parity_audit <- function(x, ...) {
  if (!inherits(x, "gp3bayes_backend_parity_audit")) {
    .gp3br_stop("`x` must be a `gp3bayes_backend_parity_audit`.")
  }
  table <- x$table
  if (!nrow(table)) return(invisible(NULL))
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(5, 8, 3, 1))
  limit <- max(abs(c(table$rstan_mean, table$cmdstanr_mean)), na.rm = TRUE)
  if (!is.finite(limit) || limit == 0) limit <- 1
  graphics::plot(
    table$rstan_mean,
    seq_len(nrow(table)),
    xlim = c(-limit, limit),
    yaxt = "n",
    ylab = "",
    xlab = "Posterior mean",
    main = "Backend posterior-summary parity",
    pch = 16,
    ...
  )
  graphics::points(
    table$cmdstanr_mean,
    seq_len(nrow(table)),
    pch = 1
  )
  graphics::segments(
    table$rstan_mean,
    seq_len(nrow(table)),
    table$cmdstanr_mean,
    seq_len(nrow(table))
  )
  graphics::axis(2, at = seq_len(nrow(table)), labels = table$variable, las = 1)
  graphics::legend(
    "topright",
    legend = c("rstan", "cmdstanr"),
    pch = c(16, 1),
    bty = "n"
  )
  invisible(x)
}

.gp3br_schema_node <- function(x, path, depth, max_depth) {
  classes <- class(x)
  type <- typeof(x)
  names_value <- if (is.list(x) || is.data.frame(x)) names(x) else character()
  row <- data.frame(
    path = path,
    class = paste(classes, collapse = "/"),
    typeof = type,
    length = length(x),
    names = paste(names_value %||% character(), collapse = "|"),
    stringsAsFactors = FALSE
  )
  if (depth >= max_depth || !is.list(x) || is.data.frame(x) || !length(x)) {
    return(row)
  }
  children <- lapply(
    seq_along(x),
    function(index) {
      name <- names(x)[index]
      if (is.null(name) || is.na(name) || !nzchar(name)) name <- as.character(index)
      .gp3br_schema_node(
        x[[index]],
        paste0(path, "$", name),
        depth + 1L,
        max_depth
      )
    }
  )
  do.call(rbind, c(list(row), children))
}

#' Capture the Structural Schema of a gp3bayes Object
#'
#' Captures classes, types, lengths, and field names without storing the
#' object's values. The result is intended for release compatibility auditing,
#' not for validating numerical or statistical equivalence.
#'
#' @param x A gp3bayes object.
#' @param max_depth Maximum nested list depth to record.
#'
#' @return A `gp3bayes_object_schema`.
#' @examples
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   condition_col = "condition"
#' )
#' capture_gp3bayes_schema(contract)
#' @export
capture_gp3bayes_schema <- function(x, max_depth = 3L) {
  if (!any(grepl("^gp3bayes_", class(x)))) {
    .gp3br_stop("`x` must inherit from a gp3bayes class.")
  }
  if (!is.numeric(max_depth) || length(max_depth) != 1L || is.na(max_depth) ||
      !is.finite(max_depth) || max_depth != floor(max_depth) || max_depth < 0) {
    .gp3br_stop("`max_depth` must be one non-negative integer.")
  }
  max_depth <- as.integer(max_depth)
  table <- .gp3br_schema_node(x, "root", 0L, max_depth)
  structure(
    list(
      schema_version = "0.2",
      object_class = class(x),
      max_depth = max_depth,
      fields = table,
      values_recorded = FALSE
    ),
    class = "gp3bayes_object_schema"
  )
}

#' Compare gp3bayes Object Schemas
#'
#' @param x A gp3bayes object or captured schema.
#' @param y A gp3bayes object or captured schema.
#' @param compare_lengths Whether vector/list lengths are part of the structural
#'   compatibility rule. The default is `FALSE` because analysis-specific
#'   cardinalities such as numbers of predictors or groups may legitimately
#'   differ while the serialized object contract remains compatible.
#'
#' @return A `gp3bayes_schema_comparison`.
#' @export
compare_gp3bayes_schemas <- function(x, y, compare_lengths = FALSE) {
  compare_lengths <- .gp3br_flag(compare_lengths, "compare_lengths")
  if (!inherits(x, "gp3bayes_object_schema")) x <- capture_gp3bayes_schema(x)
  if (!inherits(y, "gp3bayes_object_schema")) y <- capture_gp3bayes_schema(y)

  left <- x$fields
  right <- y$fields
  paths <- union(left$path, right$path)
  rows <- lapply(paths, function(path) {
    lx <- left[left$path == path, , drop = FALSE]
    ry <- right[right$path == path, , drop = FALSE]
    left_present <- nrow(lx) == 1L
    right_present <- nrow(ry) == 1L
    same_class <- left_present && right_present && identical(lx$class, ry$class)
    same_type <- left_present && right_present && identical(lx$typeof, ry$typeof)
    same_names <- left_present && right_present && identical(lx$names, ry$names)
    same_length <- left_present && right_present && identical(lx$length, ry$length)
    data.frame(
      path = path,
      reference_present = left_present,
      candidate_present = right_present,
      same_class = same_class,
      same_type = same_type,
      same_names = same_names,
      same_length = same_length,
      status = if (left_present && right_present && same_class && same_type &&
                   same_names && (!compare_lengths || same_length)) {
        "pass"
      } else {
        "review"
      },
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  status <- if (all(table$status == "pass")) "pass" else "review"
  structure(
    list(
      comparison_version = "0.2",
      status = status,
      table = table,
      reference_class = x$object_class,
      candidate_class = y$object_class,
      compare_lengths = compare_lengths,
      value_equivalence_tested = FALSE
    ),
    class = "gp3bayes_schema_comparison"
  )
}

#' Validate an Object Against a Frozen gp3bayes Schema
#'
#' @param x A gp3bayes object.
#' @param schema A `gp3bayes_object_schema`.
#' @param strict Whether structural drift should raise an error.
#' @param compare_lengths Whether analysis-specific object lengths should be
#'   required to match the frozen schema.
#'
#' @return A `gp3bayes_schema_validation` object.
#' @export
validate_gp3bayes_schema <- function(
  x, schema, strict = FALSE, compare_lengths = FALSE
) {
  strict <- .gp3br_flag(strict, "strict")
  compare_lengths <- .gp3br_flag(compare_lengths, "compare_lengths")
  if (!inherits(schema, "gp3bayes_object_schema")) {
    .gp3br_stop("`schema` must be a `gp3bayes_object_schema`.")
  }
  candidate <- capture_gp3bayes_schema(x, max_depth = schema$max_depth)
  comparison <- compare_gp3bayes_schemas(
    schema, candidate, compare_lengths = compare_lengths
  )
  result <- structure(
    list(
      validation_version = "0.2",
      status = comparison$status,
      schema = schema,
      candidate = candidate,
      comparison = comparison,
      schema_compatibility_only = TRUE
    ),
    class = "gp3bayes_schema_validation"
  )
  if (strict && identical(result$status, "review")) {
    .gp3br_stop("The object structure differs from the supplied schema.")
  }
  result
}

#' Freeze a gp3bayes Object Schema
#'
#' Marks a captured schema as frozen and optionally writes it to an explicit
#' RDS path. When `file = NULL`, no file is written.
#'
#' @param schema A captured schema or gp3bayes object.
#' @param file Optional explicit `.rds` path.
#' @param overwrite Whether an existing file may be replaced.
#'
#' @return The frozen schema, invisibly when written.
#' @export
freeze_gp3bayes_schema <- function(schema, file = NULL, overwrite = FALSE) {
  overwrite <- .gp3br_flag(overwrite, "overwrite")
  if (!inherits(schema, "gp3bayes_object_schema")) {
    schema <- capture_gp3bayes_schema(schema)
  }
  schema$frozen <- TRUE
  schema$frozen_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  class(schema) <- "gp3bayes_object_schema"

  if (is.null(file)) return(schema)
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3br_stop("`file` must be NULL or one non-empty path.")
  }
  if (file.exists(file) && !overwrite) {
    .gp3br_stop("The schema file already exists; use `overwrite = TRUE` to replace it.")
  }
  parent <- dirname(file)
  if (!dir.exists(parent)) {
    .gp3br_stop("The parent directory of `file` does not exist.")
  }
  saveRDS(schema, file = file, version = 3)
  invisible(schema)
}

#' Read a Frozen gp3bayes Object Schema
#'
#' @param file Explicit `.rds` schema path.
#' @return A `gp3bayes_object_schema`.
#' @export
read_gp3bayes_schema <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    .gp3br_stop("`file` must be one non-empty path.")
  }
  if (!file.exists(file)) .gp3br_stop("Schema file does not exist: ", file)
  schema <- readRDS(file)
  if (!inherits(schema, "gp3bayes_object_schema")) {
    .gp3br_stop("The file does not contain a gp3bayes object schema.")
  }
  schema
}

#' @export
print.gp3bayes_object_schema <- function(x, ...) {
  cat("<gp3bayes_object_schema>\n")
  cat("  Object class: ", paste(x$object_class, collapse = ", "), "\n", sep = "")
  cat("  Recorded nodes: ", nrow(x$fields), "\n", sep = "")
  cat("  Maximum depth: ", x$max_depth, "\n", sep = "")
  cat("  Values recorded: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_schema_comparison <- function(x, ...) {
  cat("<gp3bayes_schema_comparison>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Nodes reviewed: ", nrow(x$table), "\n", sep = "")
  cat("  Structural differences: ", sum(x$table$status == "review"), "\n", sep = "")
  invisible(x)
}

#' @export
plot.gp3bayes_schema_comparison <- function(x, ...) {
  if (!inherits(x, "gp3bayes_schema_comparison")) {
    .gp3br_stop("`x` must be a `gp3bayes_schema_comparison`.")
  }
  changed <- x$table$status == "review"
  values <- as.integer(changed)
  graphics::barplot(
    values,
    names.arg = x$table$path,
    las = 2,
    ylim = c(0, 1),
    ylab = "Structural difference (0/1)",
    main = "gp3bayes object-schema comparison",
    ...
  )
  invisible(x)
}

#' @export
print.gp3bayes_schema_validation <- function(x, ...) {
  cat("<gp3bayes_schema_validation>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Schema compatibility only: TRUE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_backend_environment <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$checks, row.names = row.names, optional = optional, ...)
}

#' @export
as.data.frame.gp3bayes_backend_parity_audit <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}

#' @export
as.data.frame.gp3bayes_schema_comparison <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}

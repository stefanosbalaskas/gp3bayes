# Specification-closure infrastructure for gp3bayes
#
# This file deliberately stays inside the two approved model families. It adds
# stricter observable-data audits, exact transformation replay, first-class
# estimands, governed sensitivity plans, richer posterior predictive checks,
# and an optional exact K-fold adapter. None of these functions selects a model
# automatically or converts a descriptive diagnostic into an adequacy claim.

.gp3c_stop <- function(...) {
  stop(..., call. = FALSE)
}

.gp3c_or <- function(x, y) {
  if (is.null(x)) y else x
}

.gp3c_require <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .gp3c_stop(
      "Optional package `", package, "` is required to ", purpose, "."
    )
  }
  invisible(TRUE)
}

.gp3c_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3c_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3c_number <- function(
  x,
  name,
  lower = -Inf,
  upper = Inf,
  lower_open = FALSE,
  upper_open = FALSE
) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .gp3c_stop("`", name, "` must be one finite numeric value.")
  }
  if ((lower_open && x <= lower) || (!lower_open && x < lower)) {
    .gp3c_stop("`", name, "` is below its allowed lower bound.")
  }
  if ((upper_open && x >= upper) || (!upper_open && x > upper)) {
    .gp3c_stop("`", name, "` is above its allowed upper bound.")
  }
  as.numeric(x)
}

.gp3c_integer <- function(x, name, minimum = 0L) {
  value <- .gp3c_number(x, name, lower = minimum)
  if (abs(value - round(value)) > sqrt(.Machine$double.eps)) {
    .gp3c_stop("`", name, "` must be an integer.")
  }
  as.integer(round(value))
}

.gp3c_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .gp3c_stop("`", name, "` must be one non-empty character value.")
  }
  x
}

.gp3c_contract <- function(contract, family = NULL) {
  if (!inherits(contract, "gp3bayes_model_contract")) {
    .gp3c_stop("`contract` must be a `gp3bayes_model_contract`.")
  }
  if (!is.null(family) && !identical(contract$family, family)) {
    .gp3c_stop("The model contract must use family `", family, "`.")
  }
  contract
}

.gp3c_check_row <- function(
  check_id,
  category,
  status,
  message,
  n_affected = NA_integer_
) {
  if (!status %in% c("pass", "warn", "fail", "not_applicable")) {
    .gp3c_stop("Internal error: unsupported specification-closure status.")
  }
  data.frame(
    check_id = as.character(check_id),
    category = as.character(category),
    status = as.character(status),
    message = as.character(message),
    n_affected = as.integer(n_affected),
    stringsAsFactors = FALSE
  )
}

.gp3c_worst_status <- function(status) {
  status <- as.character(status)
  if (any(status == "fail")) return("fail")
  if (any(status == "warn")) return("review")
  if (all(status == "not_applicable")) return("not_applicable")
  "pass"
}

.gp3c_fixed_formula <- function(contract) {
  .gp3c_contract(contract)
  mappings <- contract$mappings
  terms <- unique(c(
    mappings$condition,
    mappings$time,
    contract$predictors
  ))
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (!is.null(contract$interaction)) {
    interaction_term <- paste(contract$interaction, collapse = ":")
    terms <- unique(c(terms, interaction_term))
  }
  stats::reformulate(
    termlabels = terms,
    response = mappings$outcome
  )
}

.gp3c_with_seed <- function(seed, expr) {
  seed <- .gp3c_integer(seed, "seed", 0L)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

.gp3c_fit <- function(fit, family = NULL) {
  if (!inherits(fit, "gp3bayes_fit")) {
    .gp3c_stop("`fit` must inherit from `gp3bayes_fit`.")
  }
  if (!is.null(family) && !identical(fit$family, family)) {
    .gp3c_stop("The fit must use family `", family, "`.")
  }
  if (is.null(fit$backend_fit)) {
    .gp3c_stop("The gp3bayes fit does not contain a backend fit.")
  }
  fit
}

.gp3c_draw_ids <- function(backend_fit, ndraws = NULL) {
  .gp3c_require("posterior", "determine posterior draw indices")
  total <- posterior::ndraws(posterior::as_draws_array(backend_fit))
  if (is.null(ndraws)) {
    return(seq_len(total))
  }
  ndraws <- .gp3c_integer(ndraws, "ndraws", 1L)
  seq_len(min(ndraws, total))
}

.gp3c_condition_metadata <- function(prepared) {
  condition_col <- prepared$contract$mappings$condition
  if (is.null(condition_col)) {
    .gp3c_stop("The approved contract does not declare a focal condition.")
  }
  transform <- prepared$transformations$condition
  if (is.null(transform) || is.null(transform$coding)) {
    .gp3c_stop("Prepared data do not contain recorded condition coding.")
  }
  coding <- as.numeric(transform$coding)
  names(coding) <- names(transform$coding)
  if (length(coding) != 2L || anyNA(coding) || any(!is.finite(coding))) {
    .gp3c_stop("Recorded condition coding is not a valid two-level coding.")
  }
  source_levels <- transform$source_levels
  if (is.null(source_levels)) source_levels <- names(coding)
  list(
    column = condition_col,
    source_levels = as.character(source_levels),
    coding = coding,
    reference = coding[[1L]],
    focal = coding[[2L]]
  )
}

.gp3c_summary_vector <- function(x, probs = c(0.025, 0.5, 0.975)) {
  if (!is.numeric(x) || length(x) < 2L || anyNA(x) || any(!is.finite(x))) {
    .gp3c_stop("Estimand draws must be a finite numeric vector with at least two values.")
  }
  qs <- stats::quantile(x, probs = probs, names = FALSE, type = 8)
  data.frame(
    mean = mean(x),
    sd = stats::sd(x),
    median = stats::median(x),
    lower = qs[[1L]],
    middle = qs[[2L]],
    upper = qs[[3L]],
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Strict readiness components
# -------------------------------------------------------------------------

#' Summarise Overall Condition Balance
#'
#' Computes the observed proportion of each focal-condition level and applies
#' explicit review and failure thresholds. The thresholds are workflow
#' thresholds, not universal statistical laws.
#'
#' @param data A data frame.
#' @param contract An approved model contract.
#' @param warning_fraction Minimum condition fraction below which review is
#'   requested.
#' @param failure_fraction Minimum condition fraction below which the strict
#'   readiness gate fails.
#'
#' @return A `gp3bayes_condition_balance` object.
#' @export
summarise_condition_balance <- function(
  data,
  contract,
  warning_fraction = 0.10,
  failure_fraction = 0.02
) {
  .gp3c_contract(contract)
  warning_fraction <- .gp3c_number(
    warning_fraction, "warning_fraction", 0, 0.5, TRUE, FALSE
  )
  failure_fraction <- .gp3c_number(
    failure_fraction, "failure_fraction", 0, warning_fraction, FALSE, TRUE
  )
  condition_col <- contract$mappings$condition
  if (is.null(condition_col)) {
    return(structure(list(
      status = "not_applicable",
      table = data.frame(),
      minimum_fraction = NA_real_,
      warning_fraction = warning_fraction,
      failure_fraction = failure_fraction,
      interpretation = "No focal condition is declared in the model contract."
    ), class = "gp3bayes_condition_balance"))
  }
  if (!condition_col %in% names(data)) {
    .gp3c_stop("Condition column `", condition_col, "` is not present in `data`.")
  }
  values <- data[[condition_col]]
  observed <- values[!is.na(values)]
  if (length(observed) == 0L) {
    tab <- data.frame(level = character(), n = integer(), fraction = numeric())
    min_fraction <- 0
    status <- "fail"
  } else {
    counts <- table(observed, useNA = "no")
    tab <- data.frame(
      level = names(counts),
      n = as.integer(counts),
      fraction = as.numeric(counts) / sum(counts),
      stringsAsFactors = FALSE
    )
    min_fraction <- min(tab$fraction)
    status <- if (nrow(tab) != 2L || min_fraction < failure_fraction) {
      "fail"
    } else if (min_fraction < warning_fraction) {
      "review"
    } else {
      "pass"
    }
  }
  structure(list(
    status = status,
    table = tab,
    minimum_fraction = min_fraction,
    warning_fraction = warning_fraction,
    failure_fraction = failure_fraction,
    interpretation = paste(
      "Condition balance is an observable design diagnostic.",
      "It does not by itself establish identifiability or adequacy."
    )
  ), class = "gp3bayes_condition_balance")
}

#' Summarise Binary Outcome Variation Within Groups
#'
#' Identifies participants or items whose observed binary outcomes are all zero
#' or all one. Such groups are retained and reported; they are not deleted.
#'
#' @param data A data frame.
#' @param contract An approved binary model contract.
#' @param group Either `"participant"` or `"item"`.
#'
#' @return A `gp3bayes_binary_group_variation` object.
#' @export
summarise_binary_group_variation <- function(
  data,
  contract,
  group = c("participant", "item")
) {
  .gp3c_contract(contract, "binary")
  group <- match.arg(group)
  group_col <- if (group == "participant") {
    contract$mappings$participant
  } else {
    contract$mappings$item
  }
  if (is.null(group_col)) {
    return(structure(list(
      status = "not_applicable",
      group = group,
      group_column = NULL,
      table = data.frame(),
      n_no_variation = 0L,
      interpretation = "The requested grouping variable is not declared."
    ), class = "gp3bayes_binary_group_variation"))
  }
  outcome_col <- contract$mappings$outcome
  required <- c(group_col, outcome_col)
  if (!all(required %in% names(data))) {
    .gp3c_stop("Binary group-variation audit requires columns: ",
               paste(required, collapse = ", "), ".")
  }
  groups <- as.character(data[[group_col]])
  y <- data[[outcome_col]]
  split_y <- split(y, groups, drop = TRUE)
  rows <- lapply(names(split_y), function(id) {
    values <- split_y[[id]]
    values <- values[!is.na(values)]
    n0 <- sum(values == 0)
    n1 <- sum(values == 1)
    unique_values <- unique(values)
    data.frame(
      group_id = id,
      n = length(values),
      n_zero = n0,
      n_one = n1,
      variation = length(unique_values) > 1L,
      pattern = if (length(values) == 0L) {
        "missing"
      } else if (all(values == 0)) {
        "all_zero"
      } else if (all(values == 1)) {
        "all_one"
      } else {
        "variable"
      },
      stringsAsFactors = FALSE
    )
  })
  table_out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  n_no <- if (nrow(table_out)) sum(!table_out$variation) else 0L
  status <- if (nrow(table_out) == 0L) {
    "fail"
  } else if (n_no == nrow(table_out)) {
    "fail"
  } else if (n_no > 0L) {
    "review"
  } else {
    "pass"
  }
  structure(list(
    status = status,
    group = group,
    group_column = group_col,
    table = table_out,
    n_no_variation = as.integer(n_no),
    fraction_no_variation = if (nrow(table_out)) n_no / nrow(table_out) else NA_real_,
    interpretation = paste(
      "Groups without observed binary variation are reported for design review.",
      "They are not automatically excluded."
    )
  ), class = "gp3bayes_binary_group_variation")
}

#' Identify Identifier-Like Numeric Predictors
#'
#' Applies conservative heuristics to declared numeric predictors. A flagged
#' predictor is a review signal only; explicit declaration in a contract is
#' never silently overridden.
#'
#' @param data A data frame.
#' @param contract An approved model contract.
#' @param unique_fraction Fraction of rows that must be unique before a
#'   predictor can be considered identifier-like.
#' @param integer_fraction Fraction of finite values that must be integer-like.
#' @param monotone_correlation Absolute correlation with row order used as a
#'   heuristic for sequence-like identifiers.
#'
#' @return A `gp3bayes_identifier_predictor_audit` object.
#' @export
identify_identifier_like_predictors <- function(
  data,
  contract,
  unique_fraction = 0.90,
  integer_fraction = 0.98,
  monotone_correlation = 0.98
) {
  .gp3c_contract(contract)
  unique_fraction <- .gp3c_number(unique_fraction, "unique_fraction", 0, 1)
  integer_fraction <- .gp3c_number(integer_fraction, "integer_fraction", 0, 1)
  monotone_correlation <- .gp3c_number(
    monotone_correlation, "monotone_correlation", 0, 1
  )
  predictors <- contract$predictors
  if (length(predictors) == 0L) {
    return(structure(list(
      status = "pass",
      table = data.frame(),
      flagged = character(),
      interpretation = "No declared predictors require identifier-like review."
    ), class = "gp3bayes_identifier_predictor_audit"))
  }
  missing <- setdiff(predictors, names(data))
  if (length(missing)) {
    .gp3c_stop("Declared predictors are missing: ", paste(missing, collapse = ", "), ".")
  }
  name_pattern <- "(^|_)(id|index|row|participant|subject|item|stimulus|trial)(_|$)"
  rows <- lapply(predictors, function(column) {
    x <- data[[column]]
    if (!is.numeric(x)) {
      return(data.frame(
        predictor = column,
        numeric = FALSE,
        unique_fraction = NA_real_,
        integer_fraction = NA_real_,
        row_order_correlation = NA_real_,
        name_hint = grepl(name_pattern, tolower(column), perl = TRUE),
        flagged = FALSE,
        reason = "non_numeric",
        stringsAsFactors = FALSE
      ))
    }
    finite <- is.finite(x)
    xf <- x[finite]
    uf <- if (length(xf)) length(unique(xf)) / length(xf) else 0
    inf <- if (length(xf)) mean(abs(xf - round(xf)) < 1e-8) else 0
    cor_order <- if (length(xf) >= 3L && stats::sd(xf) > 0) {
      suppressWarnings(abs(stats::cor(xf, which(finite))))
    } else {
      NA_real_
    }
    hint <- grepl(name_pattern, tolower(column), perl = TRUE)
    flagged <- isTRUE(
      uf >= unique_fraction &&
        inf >= integer_fraction &&
        (hint || (is.finite(cor_order) && cor_order >= monotone_correlation))
    )
    reasons <- c(
      if (uf >= unique_fraction) "high_uniqueness" else NULL,
      if (inf >= integer_fraction) "integer_like" else NULL,
      if (hint) "identifier_name" else NULL,
      if (is.finite(cor_order) && cor_order >= monotone_correlation) "row_order_like" else NULL
    )
    data.frame(
      predictor = column,
      numeric = TRUE,
      unique_fraction = uf,
      integer_fraction = inf,
      row_order_correlation = cor_order,
      name_hint = hint,
      flagged = flagged,
      reason = if (length(reasons)) paste(reasons, collapse = ";") else "none",
      stringsAsFactors = FALSE
    )
  })
  table_out <- do.call(rbind, rows)
  flagged <- table_out$predictor[table_out$flagged]
  structure(list(
    status = if (length(flagged)) "review" else "pass",
    table = table_out,
    flagged = flagged,
    thresholds = list(
      unique_fraction = unique_fraction,
      integer_fraction = integer_fraction,
      monotone_correlation = monotone_correlation
    ),
    interpretation = paste(
      "Identifier-like detection is heuristic and conservative.",
      "A flagged predictor requires substantive review rather than automatic removal."
    )
  ), class = "gp3bayes_identifier_predictor_audit")
}

#' Review Extreme Positive Durations Without Deleting Them
#'
#' Flags observations that are extreme on the log-duration scale using both a
#' robust MAD rule and an outer-IQR rule. The function never deletes values and
#' never changes the model family automatically.
#'
#' @param data A data frame.
#' @param contract An approved duration contract.
#' @param mad_cutoff Robust absolute z-score cutoff on log durations.
#' @param iqr_multiplier Multiplier for the outer-IQR rule on log durations.
#'
#' @return A `gp3bayes_duration_extreme_review` object.
#' @export
review_duration_extremes <- function(
  data,
  contract,
  mad_cutoff = 4,
  iqr_multiplier = 3
) {
  .gp3c_contract(contract, "duration")
  mad_cutoff <- .gp3c_number(mad_cutoff, "mad_cutoff", 0, Inf, TRUE)
  iqr_multiplier <- .gp3c_number(iqr_multiplier, "iqr_multiplier", 0, Inf, TRUE)
  outcome <- contract$mappings$outcome
  if (!outcome %in% names(data)) {
    .gp3c_stop("Duration outcome column `", outcome, "` is not present.")
  }
  y <- suppressWarnings(as.numeric(as.character(data[[outcome]])))
  valid <- is.finite(y) & y > 0
  log_y <- rep(NA_real_, length(y))
  log_y[valid] <- log(y[valid])
  valid_log <- log_y[valid]
  if (length(valid_log) < 4L) {
    return(structure(list(
      status = "review",
      duration = y,
      log_duration = log_y,
      n = length(y),
      flagged = data.frame(),
      n_flagged = 0L,
      fraction_flagged = 0,
      thresholds = list(mad_cutoff = mad_cutoff, iqr_multiplier = iqr_multiplier),
      interpretation = "Too few positive finite durations are available for robust extreme-value review."
    ), class = "gp3bayes_duration_extreme_review"))
  }
  med <- stats::median(valid_log)
  mad_value <- stats::mad(valid_log, center = med, constant = 1.4826)
  robust_z <- rep(NA_real_, length(y))
  if (is.finite(mad_value) && mad_value > 0) {
    robust_z[valid] <- abs((valid_log - med) / mad_value)
  }
  qs <- stats::quantile(valid_log, c(0.25, 0.75), names = FALSE, type = 8)
  iqr <- qs[[2L]] - qs[[1L]]
  lower <- qs[[1L]] - iqr_multiplier * iqr
  upper <- qs[[2L]] + iqr_multiplier * iqr
  mad_flag <- valid & is.finite(robust_z) & robust_z > mad_cutoff
  iqr_flag <- valid & (log_y < lower | log_y > upper)
  flagged_index <- which(mad_flag | iqr_flag)
  flagged <- data.frame(
    row = flagged_index,
    duration = y[flagged_index],
    log_duration = log_y[flagged_index],
    robust_z = robust_z[flagged_index],
    mad_rule = mad_flag[flagged_index],
    outer_iqr_rule = iqr_flag[flagged_index],
    stringsAsFactors = FALSE
  )
  structure(list(
    status = if (length(flagged_index)) "review" else "pass",
    duration = y,
    log_duration = log_y,
    n = length(y),
    flagged = flagged,
    n_flagged = as.integer(length(flagged_index)),
    fraction_flagged = length(flagged_index) / length(y),
    thresholds = list(
      mad_cutoff = mad_cutoff,
      iqr_multiplier = iqr_multiplier,
      log_iqr_lower = lower,
      log_iqr_upper = upper
    ),
    interpretation = paste(
      "Extreme durations are retained and surfaced for substantive review.",
      "Extremity alone is not a deletion rule or evidence for another likelihood."
    )
  ), class = "gp3bayes_duration_extreme_review")
}

.gp3c_censor_flags <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & is.finite(x) & x != 0)
  text <- tolower(trimws(as.character(x)))
  !is.na(text) & !text %in% c(
    "", "0", "false", "f", "no", "n", "none", "uncensored", "observed", "complete"
  )
}

#' Audit Duration Range and Censoring Boundaries
#'
#' Checks an explicitly declared plausible measurement range and an explicitly
#' supplied censoring indicator. Candidate censoring-like column names are only
#' reported when no indicator is supplied; they are not interpreted silently.
#'
#' @param data A data frame.
#' @param contract An approved duration contract.
#' @param allowed_range Optional positive lower and upper bounds in the
#'   contract's recorded outcome unit.
#' @param censor_col Optional censoring-indicator column.
#' @param detect_candidate_columns Whether common censoring/truncation names
#'   should be reported for review.
#'
#' @return A `gp3bayes_duration_boundary_audit` object.
#' @export
audit_duration_boundaries <- function(
  data,
  contract,
  allowed_range = NULL,
  censor_col = NULL,
  detect_candidate_columns = TRUE
) {
  .gp3c_contract(contract, "duration")
  detect_candidate_columns <- .gp3c_flag(
    detect_candidate_columns, "detect_candidate_columns"
  )
  outcome <- contract$mappings$outcome
  if (!outcome %in% names(data)) {
    .gp3c_stop("Duration outcome column `", outcome, "` is not present.")
  }
  y <- suppressWarnings(as.numeric(as.character(data[[outcome]])))
  checks <- list()
  range_violations <- integer()
  if (is.null(allowed_range)) {
    checks[[length(checks) + 1L]] <- .gp3c_check_row(
      "declared_duration_range", "duration_boundaries", "not_applicable",
      "No impossible or plausible duration range was supplied."
    )
  } else {
    if (!is.numeric(allowed_range) || length(allowed_range) != 2L ||
        anyNA(allowed_range) || any(!is.finite(allowed_range)) ||
        allowed_range[[1L]] <= 0 || allowed_range[[1L]] >= allowed_range[[2L]]) {
      .gp3c_stop("`allowed_range` must contain increasing positive finite bounds.")
    }
    range_violations <- which(
      !is.finite(y) | y < allowed_range[[1L]] | y > allowed_range[[2L]]
    )
    checks[[length(checks) + 1L]] <- .gp3c_check_row(
      "declared_duration_range",
      "duration_boundaries",
      if (length(range_violations)) "fail" else "pass",
      if (length(range_violations)) {
        paste(length(range_violations), "durations fall outside the declared range.")
      } else {
        "All durations fall inside the declared range."
      },
      length(range_violations)
    )
  }
  candidate_columns <- character()
  censored_rows <- integer()
  if (!is.null(censor_col)) {
    censor_col <- .gp3c_string(censor_col, "censor_col")
    if (!censor_col %in% names(data)) {
      .gp3c_stop("Censoring column `", censor_col, "` is not present.")
    }
    flags <- .gp3c_censor_flags(data[[censor_col]])
    censored_rows <- which(flags)
    checks[[length(checks) + 1L]] <- .gp3c_check_row(
      "uncensored_contract",
      "duration_boundaries",
      if (length(censored_rows)) "fail" else "pass",
      if (length(censored_rows)) {
        paste(length(censored_rows), "rows are marked censored or truncated.")
      } else {
        "The supplied censoring indicator contains no censored observations."
      },
      length(censored_rows)
    )
  } else if (detect_candidate_columns) {
    candidate_columns <- names(data)[
      grepl("cens|censor|trunc|deadline", tolower(names(data)), perl = TRUE)
    ]
    checks[[length(checks) + 1L]] <- .gp3c_check_row(
      "uncensored_contract",
      "duration_boundaries",
      if (length(candidate_columns)) "warn" else "pass",
      if (length(candidate_columns)) {
        paste(
          "Potential censoring/truncation columns require explicit review:",
          paste(candidate_columns, collapse = ", ")
        )
      } else {
        "No censoring-like column names were detected."
      },
      length(candidate_columns)
    )
  } else {
    checks[[length(checks) + 1L]] <- .gp3c_check_row(
      "uncensored_contract", "duration_boundaries", "not_applicable",
      "No censoring indicator was supplied or heuristically reviewed."
    )
  }
  checks <- do.call(rbind, checks)
  structure(list(
    status = .gp3c_worst_status(checks$status),
    checks = checks,
    allowed_range = allowed_range,
    range_violations = range_violations,
    censor_col = censor_col,
    censored_rows = censored_rows,
    candidate_columns = candidate_columns,
    interpretation = paste(
      "The positive-duration contract is uncensored.",
      "A censoring signal or declared impossible value is a contract failure, not a request to refit another family automatically."
    )
  ), class = "gp3bayes_duration_boundary_audit")
}

#' Run a Strict Model-Readiness Audit
#'
#' Extends [audit_model_readiness()] with explicit overall condition balance,
#' binary within-group outcome variation, identifier-like predictor review,
#' fixed-effects rank, duration extremes and boundaries, and optional binary
#' separation screening.
#'
#' @param data A data frame.
#' @param contract An approved model contract.
#' @param condition_warning_fraction,condition_failure_fraction Condition
#'   balance thresholds.
#' @param identifier_unique_fraction Identifier-like uniqueness threshold.
#' @param duration_allowed_range Optional positive duration bounds.
#' @param censor_col Optional duration censoring indicator.
#' @param run_separation Whether to run the optional fixed-effects separation
#'   screen when the family is binary.
#'
#' @return A `gp3bayes_strict_readiness_audit` object.
#' @export
audit_model_readiness_strict <- function(
  data,
  contract,
  condition_warning_fraction = 0.10,
  condition_failure_fraction = 0.02,
  identifier_unique_fraction = 0.90,
  duration_allowed_range = NULL,
  censor_col = NULL,
  run_separation = TRUE
) {
  .gp3c_contract(contract)
  run_separation <- .gp3c_flag(run_separation, "run_separation")
  base <- audit_model_readiness(data, contract)
  extras <- list()
  balance <- summarise_condition_balance(
    data,
    contract,
    warning_fraction = condition_warning_fraction,
    failure_fraction = condition_failure_fraction
  )
  extras[[length(extras) + 1L]] <- .gp3c_check_row(
    "overall_condition_balance",
    "design",
    if (balance$status == "review") "warn" else balance$status,
    if (balance$status == "not_applicable") {
      balance$interpretation
    } else {
      paste0(
        "Minimum observed condition fraction = ",
        format(balance$minimum_fraction, digits = 4), "."
      )
    },
    if (nrow(balance$table)) min(balance$table$n) else NA_integer_
  )
  identifier <- identify_identifier_like_predictors(
    data,
    contract,
    unique_fraction = identifier_unique_fraction
  )
  extras[[length(extras) + 1L]] <- .gp3c_check_row(
    "identifier_like_predictors",
    "predictors",
    if (identifier$status == "review") "warn" else identifier$status,
    if (length(identifier$flagged)) {
      paste("Identifier-like predictors require review:", paste(identifier$flagged, collapse = ", "))
    } else {
      "No declared predictor met the identifier-like heuristic."
    },
    length(identifier$flagged)
  )
  rank_result <- tryCatch({
    mm <- stats::model.matrix(.gp3c_fixed_formula(contract), data = data)
    list(rank = qr(mm)$rank, columns = ncol(mm), error = NULL)
  }, error = function(e) list(rank = NA_integer_, columns = NA_integer_, error = conditionMessage(e)))
  rank_status <- if (!is.null(rank_result$error)) {
    "fail"
  } else if (rank_result$rank < rank_result$columns) {
    "fail"
  } else {
    "pass"
  }
  extras[[length(extras) + 1L]] <- .gp3c_check_row(
    "fixed_effect_rank",
    "design",
    rank_status,
    if (!is.null(rank_result$error)) {
      paste("The fixed-effects matrix could not be constructed:", rank_result$error)
    } else if (rank_status == "fail") {
      paste("Fixed-effects matrix rank", rank_result$rank, "is below", rank_result$columns, "columns.")
    } else {
      paste("Fixed-effects matrix has full rank", rank_result$rank, "of", rank_result$columns, ".")
    }
  )
  binary_variation <- NULL
  separation <- NULL
  duration_extremes <- NULL
  duration_boundaries <- NULL
  if (identical(contract$family, "binary")) {
    binary_variation <- summarise_binary_group_variation(data, contract, "participant")
    extras[[length(extras) + 1L]] <- .gp3c_check_row(
      "participant_binary_outcome_variation",
      "outcome",
      if (binary_variation$status == "review") "warn" else binary_variation$status,
      paste(
        binary_variation$n_no_variation,
        "participant groups have no observed binary outcome variation."
      ),
      binary_variation$n_no_variation
    )
    if (run_separation) {
      if (requireNamespace("detectseparation", quietly = TRUE)) {
        separation <- tryCatch(
          detect_binary_separation(data, formula = .gp3c_fixed_formula(contract)),
          error = function(e) e
        )
        if (inherits(separation, "error")) {
          extras[[length(extras) + 1L]] <- .gp3c_check_row(
            "fixed_effect_separation", "design", "warn",
            paste("Separation screening could not be completed:", conditionMessage(separation))
          )
        } else {
          extras[[length(extras) + 1L]] <- .gp3c_check_row(
            "fixed_effect_separation",
            "design",
            if (isTRUE(separation$separation_detected)) "warn" else "pass",
            if (isTRUE(separation$separation_detected)) {
              "The fixed-effects logistic screen detected infinite separation coefficients."
            } else {
              "The fixed-effects logistic separation screen did not detect infinite coefficients."
            }
          )
        }
      } else {
        extras[[length(extras) + 1L]] <- .gp3c_check_row(
          "fixed_effect_separation", "design", "warn",
          "`detectseparation` is not installed, so the optional separation screen was not run."
        )
      }
    }
  } else {
    duration_extremes <- review_duration_extremes(data, contract)
    extras[[length(extras) + 1L]] <- .gp3c_check_row(
      "duration_extreme_review",
      "outcome",
      if (duration_extremes$status == "review") "warn" else duration_extremes$status,
      paste(duration_extremes$n_flagged, "duration observations were flagged for extreme-value review."),
      duration_extremes$n_flagged
    )
    duration_boundaries <- audit_duration_boundaries(
      data,
      contract,
      allowed_range = duration_allowed_range,
      censor_col = censor_col
    )
    boundary_checks <- duration_boundaries$checks
    for (i in seq_len(nrow(boundary_checks))) {
      extras[[length(extras) + 1L]] <- boundary_checks[i, , drop = FALSE]
    }
  }
  extra_checks <- do.call(rbind, extras)
  base_checks <- base$checks
  common <- intersect(names(base_checks), names(extra_checks))
  combined <- rbind(
    base_checks[, common, drop = FALSE],
    extra_checks[, common, drop = FALSE]
  )
  counts <- c(
    pass = sum(combined$status == "pass"),
    warn = sum(combined$status == "warn"),
    fail = sum(combined$status == "fail"),
    not_applicable = sum(combined$status == "not_applicable")
  )
  ready <- counts[["fail"]] == 0L
  status <- if (!ready) {
    "not_ready"
  } else if (counts[["warn"]] > 0L) {
    "ready_with_warnings"
  } else {
    "ready"
  }
  structure(list(
    audit_version = "0.2",
    family = contract$family,
    ready = ready,
    status = status,
    status_counts = counts,
    checks = combined,
    base_audit = base,
    condition_balance = balance,
    binary_group_variation = binary_variation,
    identifier_audit = identifier,
    rank = rank_result,
    separation = separation,
    duration_extremes = duration_extremes,
    duration_boundaries = duration_boundaries,
    contract = contract,
    thresholds = list(
      condition_warning_fraction = condition_warning_fraction,
      condition_failure_fraction = condition_failure_fraction,
      identifier_unique_fraction = identifier_unique_fraction,
      duration_allowed_range = duration_allowed_range
    ),
    interpretation = paste(
      "Strict readiness is an observable-data gate.",
      "Passing does not establish convergence, posterior adequacy, predictive validity, or causal identification."
    )
  ), class = "gp3bayes_strict_readiness_audit")
}

#' @export
print.gp3bayes_strict_readiness_audit <- function(x, ...) {
  cat("\nStrict gp3bayes readiness audit\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Status: ", x$status, "\n", sep = "")
  cat(" Ready: ", x$ready, "\n", sep = "")
  cat(
    " Checks: ", x$status_counts[["pass"]], " passed, ",
    x$status_counts[["warn"]], " warnings, ",
    x$status_counts[["fail"]], " failures\n",
    sep = ""
  )
  issues <- x$checks[x$checks$status %in% c("warn", "fail"), , drop = FALSE]
  if (nrow(issues)) {
    cat(" Issues:\n")
    for (i in seq_len(nrow(issues))) {
      cat("  [", toupper(issues$status[[i]]), "] ", issues$check_id[[i]],
          ": ", issues$message[[i]], "\n", sep = "")
    }
  }
  invisible(x)
}

#' Plot a Strict Readiness Audit
#'
#' @param x A `gp3bayes_strict_readiness_audit`.
#' @param type One of `"status"`, `"condition"`, or `"duration_extremes"`.
#' @param ... Additional base-graphics arguments.
#' @return `x`, invisibly.
#' @export
plot.gp3bayes_strict_readiness_audit <- function(
  x,
  type = c("status", "condition", "duration_extremes"),
  ...
) {
  type <- match.arg(type)
  if (type == "status") {
    values <- x$status_counts[c("pass", "warn", "fail")]
    graphics::barplot(values, ylab = "Number of checks", main = "Strict readiness checks", ...)
  } else if (type == "condition") {
    tab <- x$condition_balance$table
    if (!nrow(tab)) .gp3c_stop("No condition-balance table is available to plot.")
    graphics::barplot(tab$fraction, names.arg = tab$level, ylim = c(0, 1),
                      ylab = "Observed fraction", main = "Condition balance", ...)
  } else {
    review <- x$duration_extremes
    if (is.null(review) || !nrow(review$flagged)) {
      .gp3c_stop("No flagged duration extremes are available to plot.")
    }
    graphics::plot(
      review$flagged$row,
      review$flagged$log_duration,
      xlab = "Row",
      ylab = "Log duration",
      main = "Flagged duration observations",
      ...
    )
  }
  invisible(x)
}

# -------------------------------------------------------------------------
# Transformation replay
# -------------------------------------------------------------------------

#' Create a Reusable Transformation Recipe
#'
#' Extracts the recorded condition coding, outcome mapping or unit conversion,
#' numeric scaling registry, fixed-effects formula, and model-matrix columns
#' from a prepared gp3bayes object.
#'
#' @param prepared A binary or duration prepared object.
#' @return A `gp3bayes_transformation_recipe`.
#' @export
create_transformation_recipe <- function(prepared) {
  if (!inherits(prepared, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    .gp3c_stop("`prepared` must be a gp3bayes binary or duration prepared object.")
  }
  family <- prepared$contract$family
  structure(list(
    recipe_version = "0.2",
    family = family,
    contract = prepared$contract,
    transformations = prepared$transformations,
    fixed_formula = prepared$fixed_formula,
    fixed_formula_text = prepared$fixed_formula_text,
    model_matrix_columns = prepared$model_matrix_columns,
    outcome_unit = if (identical(family, "duration")) prepared$outcome_unit else NULL,
    source_preparation_version = prepared$preparation_version,
    interpretation = paste(
      "This recipe replays only transformations recorded by gp3bayes.",
      "It does not infer new scaling, recode unseen levels, or repair missing data silently."
    )
  ), class = "gp3bayes_transformation_recipe")
}

.gp3c_binary_scale_registry <- function(recipe) {
  recipe$transformations$numeric_scaling
}

.gp3c_duration_scale_registry <- function(recipe) {
  recipe$transformations$scaled_columns
}

#' Apply a Recorded Transformation Recipe
#'
#' @param new_data New data to transform.
#' @param recipe A `gp3bayes_transformation_recipe` or prepared gp3bayes object.
#' @param input_scale Either `"raw"` or `"prepared"`.
#' @param require_outcome Whether the outcome column must be present.
#' @param input_unit Optional source duration unit used to guard duration replay.
#'
#' @return A transformed data frame with a recipe attribute.
#' @export
apply_transformation_recipe <- function(
  new_data,
  recipe,
  input_scale = c("raw", "prepared"),
  require_outcome = FALSE,
  input_unit = NULL
) {
  if (!is.data.frame(new_data)) .gp3c_stop("`new_data` must be a data frame.")
  if (inherits(recipe, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    recipe <- create_transformation_recipe(recipe)
  }
  if (!inherits(recipe, "gp3bayes_transformation_recipe")) {
    .gp3c_stop("`recipe` must be a transformation recipe or prepared object.")
  }
  input_scale <- match.arg(input_scale)
  require_outcome <- .gp3c_flag(require_outcome, "require_outcome")
  data <- new_data
  contract <- recipe$contract
  outcome <- contract$mappings$outcome
  if (require_outcome && !outcome %in% names(data)) {
    .gp3c_stop("Outcome column `", outcome, "` is required but absent.")
  }
  if (input_scale == "prepared") {
    attr(data, "gp3bayes_transformation_recipe") <- recipe
    return(data)
  }
  if (identical(recipe$family, "binary")) {
    outcome_transform <- recipe$transformations$outcome
    if (outcome %in% names(data)) {
      mapping <- outcome_transform$mapping
      keys <- names(mapping)
      source <- as.character(data[[outcome]])
      mapped <- unname(mapping[match(source, keys)])
      already <- suppressWarnings(as.numeric(source))
      use_already <- is.na(mapped) & !is.na(already) & already %in% c(0, 1)
      mapped[use_already] <- already[use_already]
      if (anyNA(mapped)) {
        .gp3c_stop("New binary outcome contains values absent from the recorded mapping.")
      }
      data[[outcome]] <- as.integer(mapped)
    }
    condition <- recipe$transformations$condition
    if (!is.null(condition)) {
      column <- condition$column
      if (is.null(column)) column <- contract$mappings$condition
      if (!column %in% names(data)) .gp3c_stop("Condition column `", column, "` is absent.")
      source <- as.character(data[[column]])
      coding <- condition$coding
      mapped <- unname(coding[match(source, names(coding))])
      numeric_source <- suppressWarnings(as.numeric(source))
      valid_codes <- as.numeric(coding)
      use_already <- is.na(mapped) & !is.na(numeric_source) & numeric_source %in% valid_codes
      mapped[use_already] <- numeric_source[use_already]
      if (anyNA(mapped)) {
        .gp3c_stop("New data contain condition values absent from the recorded coding.")
      }
      data[[column]] <- as.numeric(mapped)
    }
    registry <- .gp3c_binary_scale_registry(recipe)
    for (column in names(registry)) {
      if (!column %in% names(data)) .gp3c_stop("Scaled predictor `", column, "` is absent.")
      values <- data[[column]]
      if (!is.numeric(values) || any(!is.finite(values))) {
        .gp3c_stop("Scaled predictor `", column, "` must be finite and numeric.")
      }
      center <- unname(registry[[column]][["center"]])
      scale <- unname(registry[[column]][["scale"]])
      data[[column]] <- (values - center) / scale
    }
  } else {
    outcome_transform <- recipe$transformations$outcome
    source_unit <- outcome_transform$source_unit
    if (!is.null(input_unit)) {
      input_unit <- .gp3c_string(input_unit, "input_unit")
      if (!identical(input_unit, source_unit)) {
        .gp3c_stop("`input_unit` does not match the recipe source unit `", source_unit, "`.")
      }
    }
    if (outcome %in% names(data)) {
      y <- suppressWarnings(as.numeric(as.character(data[[outcome]])))
      if (anyNA(y) || any(!is.finite(y)) || any(y <= 0)) {
        .gp3c_stop("New duration outcomes must be finite and strictly positive.")
      }
      data[[outcome]] <- y * outcome_transform$multiplier
    }
    condition <- recipe$transformations$condition
    if (!is.null(condition)) {
      column <- contract$mappings$condition
      if (!column %in% names(data)) .gp3c_stop("Condition column `", column, "` is absent.")
      source <- as.character(data[[column]])
      coding <- condition$coding
      mapped <- unname(coding[match(source, names(coding))])
      numeric_source <- suppressWarnings(as.numeric(source))
      valid_codes <- as.numeric(coding)
      use_already <- is.na(mapped) & !is.na(numeric_source) & numeric_source %in% valid_codes
      mapped[use_already] <- numeric_source[use_already]
      if (anyNA(mapped)) {
        .gp3c_stop("New data contain condition values absent from the recorded coding.")
      }
      data[[column]] <- as.numeric(mapped)
    }
    registry <- .gp3c_duration_scale_registry(recipe)
    for (column in names(registry)) {
      if (!column %in% names(data)) .gp3c_stop("Scaled predictor `", column, "` is absent.")
      values <- data[[column]]
      if (!is.numeric(values) || any(!is.finite(values))) {
        .gp3c_stop("Scaled predictor `", column, "` must be finite and numeric.")
      }
      center <- registry[[column]]$centre
      scale <- registry[[column]]$scale
      data[[column]] <- (values - center) / scale
    }
  }
  attr(data, "gp3bayes_transformation_recipe") <- recipe
  data
}

#' Invert a Recorded Transformation Recipe
#'
#' Reconstructs a raw-scale representation from prepared data when the
#' recorded transformations are invertible. This is intended for replay tests
#' and controlled sensitivity construction, not recovery of discarded rows.
#'
#' @param data Prepared-scale data.
#' @param recipe A transformation recipe or prepared object.
#' @return A raw-scale data frame.
#' @export
invert_transformation_recipe <- function(data, recipe) {
  if (!is.data.frame(data)) .gp3c_stop("`data` must be a data frame.")
  if (inherits(recipe, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    recipe <- create_transformation_recipe(recipe)
  }
  if (!inherits(recipe, "gp3bayes_transformation_recipe")) {
    .gp3c_stop("`recipe` must be a transformation recipe or prepared object.")
  }
  out <- data
  contract <- recipe$contract
  outcome <- contract$mappings$outcome
  if (identical(recipe$family, "binary")) {
    registry <- .gp3c_binary_scale_registry(recipe)
    for (column in names(registry)) {
      out[[column]] <- out[[column]] * registry[[column]][["scale"]] +
        registry[[column]][["center"]]
    }
    condition <- recipe$transformations$condition
    if (!is.null(condition)) {
      column <- condition$column
      if (is.null(column)) column <- contract$mappings$condition
      coding <- as.numeric(condition$coding)
      labels <- names(condition$coding)
      idx <- match(out[[column]], coding)
      if (anyNA(idx)) .gp3c_stop("Prepared condition values do not match the recorded coding.")
      out[[column]] <- labels[idx]
    }
    if (outcome %in% names(out)) {
      mapping <- recipe$transformations$outcome$mapping
      inverse <- stats::setNames(names(mapping), as.character(mapping))
      key <- as.character(out[[outcome]])
      restored <- unname(inverse[key])
      if (anyNA(restored)) .gp3c_stop("Prepared outcome values do not match the recorded mapping.")
      out[[outcome]] <- restored
    }
  } else {
    registry <- .gp3c_duration_scale_registry(recipe)
    for (column in names(registry)) {
      out[[column]] <- out[[column]] * registry[[column]]$scale + registry[[column]]$centre
    }
    condition <- recipe$transformations$condition
    if (!is.null(condition)) {
      column <- contract$mappings$condition
      coding <- as.numeric(condition$coding)
      labels <- names(condition$coding)
      idx <- match(out[[column]], coding)
      if (anyNA(idx)) .gp3c_stop("Prepared condition values do not match the recorded coding.")
      out[[column]] <- labels[idx]
    }
    if (outcome %in% names(out)) {
      multiplier <- recipe$transformations$outcome$multiplier
      out[[outcome]] <- out[[outcome]] / multiplier
    }
  }
  attr(out, "gp3bayes_transformation_recipe") <- NULL
  out
}

#' Validate Exact Transformation Replay
#'
#' Round-trips the prepared data through the recorded inverse and forward
#' transformation and compares transformed columns and fixed-effect matrices.
#'
#' @param prepared A binary or duration prepared object.
#' @param tolerance Numeric comparison tolerance.
#' @return A `gp3bayes_transformation_replay_audit`.
#' @export
validate_transformation_replay <- function(prepared, tolerance = 1e-10) {
  tolerance <- .gp3c_number(tolerance, "tolerance", 0, Inf)
  recipe <- create_transformation_recipe(prepared)
  raw <- invert_transformation_recipe(prepared$data, recipe)
  replayed <- apply_transformation_recipe(
    raw,
    recipe,
    input_scale = "raw",
    require_outcome = TRUE,
    input_unit = if (identical(recipe$family, "duration")) {
      recipe$transformations$outcome$source_unit
    } else {
      NULL
    }
  )
  columns <- intersect(names(prepared$data), names(replayed))
  rows <- lapply(columns, function(column) {
    a <- prepared$data[[column]]
    b <- replayed[[column]]
    if (is.numeric(a) && is.numeric(b)) {
      diff <- max(abs(a - b), na.rm = TRUE)
      if (!is.finite(diff)) diff <- 0
      equal <- diff <= tolerance
    } else {
      diff <- NA_real_
      equal <- identical(as.character(a), as.character(b))
    }
    data.frame(column = column, equal = equal, max_abs_difference = diff,
               stringsAsFactors = FALSE)
  })
  comparison <- do.call(rbind, rows)
  mm_original <- stats::model.matrix(recipe$fixed_formula, data = prepared$data)
  mm_replay <- stats::model.matrix(recipe$fixed_formula, data = replayed)
  matrix_columns_equal <- identical(colnames(mm_original), colnames(mm_replay))
  matrix_difference <- if (identical(dim(mm_original), dim(mm_replay))) {
    max(abs(mm_original - mm_replay))
  } else {
    Inf
  }
  pass <- all(comparison$equal) && matrix_columns_equal && matrix_difference <= tolerance
  structure(list(
    status = if (pass) "pass" else "fail",
    family = recipe$family,
    recipe = recipe,
    column_comparison = comparison,
    model_matrix_columns_equal = matrix_columns_equal,
    model_matrix_max_abs_difference = matrix_difference,
    tolerance = tolerance,
    raw_reconstruction = raw,
    replayed_data = replayed,
    interpretation = paste(
      "A replay pass establishes deterministic transformation reproducibility for the retained rows.",
      "It does not recover rows previously removed by an explicit missing-data decision."
    )
  ), class = "gp3bayes_transformation_replay_audit")
}

#' @export
print.gp3bayes_transformation_replay_audit <- function(x, ...) {
  cat("\nTransformation replay audit\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Status: ", x$status, "\n", sep = "")
  cat(" Model-matrix columns identical: ", x$model_matrix_columns_equal, "\n", sep = "")
  cat(" Maximum model-matrix difference: ", format(x$model_matrix_max_abs_difference, digits = 6), "\n", sep = "")
  invisible(x)
}

#' @export
plot.gp3bayes_transformation_replay_audit <- function(x, ...) {
  values <- x$column_comparison$max_abs_difference
  values[!is.finite(values)] <- 0
  graphics::barplot(
    values,
    names.arg = x$column_comparison$column,
    las = 2,
    ylab = "Maximum absolute difference",
    main = "Transformation replay differences",
    ...
  )
  invisible(x)
}

# -------------------------------------------------------------------------
# First-class estimands
# -------------------------------------------------------------------------

.gp3c_target_data <- function(fit, target_data, target_scale) {
  prepared <- fit$specification$prepared
  if (is.null(target_data)) return(prepared$data)
  if (!is.data.frame(target_data)) .gp3c_stop("`target_data` must be a data frame.")
  target_scale <- match.arg(target_scale, c("prepared", "raw"))
  if (target_scale == "prepared") return(target_data)
  apply_transformation_recipe(
    target_data,
    prepared,
    input_scale = "raw",
    require_outcome = FALSE,
    input_unit = if (identical(fit$family, "duration")) {
      prepared$transformations$outcome$source_unit
    } else NULL
  )
}

.gp3c_estimand_object <- function(family, draws, primary_quantity, metadata) {
  if (!is.data.frame(draws) || !primary_quantity %in% names(draws)) {
    .gp3c_stop("Internal error: malformed estimand draws.")
  }
  structure(list(
    estimand_version = "0.2",
    family = family,
    primary_quantity = primary_quantity,
    draws = draws,
    metadata = metadata,
    automatic_decision = FALSE,
    interpretation = paste(
      "The object contains posterior draws of a prespecified estimand.",
      "Credible intervals are not automatic causal or adequacy claims."
    )
  ), class = "gp3bayes_estimand")
}

#' Estimate a Design-Standardised Binary Probability Contrast
#'
#' Replaces the focal-condition value across a declared target covariate
#' distribution, obtains population-level expected probabilities using
#' `brms::posterior_epred()`, and averages within each posterior draw.
#'
#' @param fit An approved gp3bayes binary fit.
#' @param target_data Optional target covariate distribution.
#' @param target_scale Whether supplied target data are raw or already prepared.
#' @param ndraws Optional number of posterior draws.
#' @param include_group_effects Whether recorded group-level effects are
#'   included. The default `FALSE` targets population-level predictions.
#'
#' @return A `gp3bayes_estimand` with probability-difference draws.
#' @export
estimate_standardized_probability_contrast <- function(
  fit,
  target_data = NULL,
  target_scale = c("prepared", "raw"),
  ndraws = NULL,
  include_group_effects = FALSE
) {
  .gp3c_fit(fit, "binary")
  .gp3c_require("brms", "estimate standardised binary probabilities")
  include_group_effects <- .gp3c_flag(include_group_effects, "include_group_effects")
  target_scale <- match.arg(target_scale)
  target <- .gp3c_target_data(fit, target_data, target_scale)
  condition <- .gp3c_condition_metadata(fit$specification$prepared)
  if (!condition$column %in% names(target)) {
    .gp3c_stop("Target data do not contain the focal condition column.")
  }
  reference_data <- target
  focal_data <- target
  reference_data[[condition$column]] <- condition$reference
  focal_data[[condition$column]] <- condition$focal
  ids <- .gp3c_draw_ids(fit$backend_fit, ndraws)
  re_formula <- if (include_group_effects) NULL else NA
  reference <- brms::posterior_epred(
    fit$backend_fit,
    newdata = reference_data,
    re_formula = re_formula,
    draw_ids = ids
  )
  focal <- brms::posterior_epred(
    fit$backend_fit,
    newdata = focal_data,
    re_formula = re_formula,
    draw_ids = ids
  )
  reference <- rowMeans(as.matrix(reference))
  focal <- rowMeans(as.matrix(focal))
  odds <- function(p) p / pmax(1 - p, .Machine$double.eps)
  draws <- data.frame(
    .draw = ids,
    reference_probability = reference,
    focal_probability = focal,
    probability_difference = focal - reference,
    probability_ratio = focal / pmax(reference, .Machine$double.eps),
    odds_ratio_of_standardized_probabilities = odds(focal) / pmax(odds(reference), .Machine$double.eps),
    stringsAsFactors = FALSE
  )
  .gp3c_estimand_object(
    "binary",
    draws,
    "probability_difference",
    list(
      condition_column = condition$column,
      reference_level = condition$source_levels[[1L]],
      focal_level = condition$source_levels[[2L]],
      target_rows = nrow(target),
      include_group_effects = include_group_effects,
      interpretation = "The primary estimand is a design-standardised probability difference."
    )
  )
}

#' Estimate Design-Standardised Duration Estimands
#'
#' Produces posterior draws of average conditional medians, their difference
#' and ratio, the average log-duration contrast, and a posterior predictive
#' upper quantile under each focal-condition level.
#'
#' @param fit An approved gp3bayes duration fit.
#' @param target_data Optional target covariate distribution.
#' @param target_scale Whether supplied target data are raw or prepared.
#' @param predictive_quantile Predictive quantile probability.
#' @param ndraws Optional number of posterior draws.
#' @param include_group_effects Whether group-level effects are included.
#' @param seed Seed used for posterior predictive draws.
#'
#' @return A `gp3bayes_estimand`.
#' @export
estimate_standardized_duration_estimands <- function(
  fit,
  target_data = NULL,
  target_scale = c("prepared", "raw"),
  predictive_quantile = 0.90,
  ndraws = NULL,
  include_group_effects = FALSE,
  seed = 1L
) {
  .gp3c_fit(fit, "duration")
  .gp3c_require("brms", "estimate standardised duration quantities")
  predictive_quantile <- .gp3c_number(
    predictive_quantile, "predictive_quantile", 0, 1, TRUE, TRUE
  )
  include_group_effects <- .gp3c_flag(include_group_effects, "include_group_effects")
  target_scale <- match.arg(target_scale)
  target <- .gp3c_target_data(fit, target_data, target_scale)
  condition <- .gp3c_condition_metadata(fit$specification$prepared)
  if (!condition$column %in% names(target)) {
    .gp3c_stop("Target data do not contain the focal condition column.")
  }
  reference_data <- target
  focal_data <- target
  reference_data[[condition$column]] <- condition$reference
  focal_data[[condition$column]] <- condition$focal
  ids <- .gp3c_draw_ids(fit$backend_fit, ndraws)
  re_formula <- if (include_group_effects) NULL else NA
  lin_ref <- as.matrix(brms::posterior_linpred(
    fit$backend_fit,
    newdata = reference_data,
    re_formula = re_formula,
    draw_ids = ids,
    transform = FALSE
  ))
  lin_focal <- as.matrix(brms::posterior_linpred(
    fit$backend_fit,
    newdata = focal_data,
    re_formula = re_formula,
    draw_ids = ids,
    transform = FALSE
  ))
  median_ref <- rowMeans(exp(lin_ref))
  median_focal <- rowMeans(exp(lin_focal))
  log_contrast <- rowMeans(lin_focal - lin_ref)
  pred <- .gp3c_with_seed(seed, list(
    reference = as.matrix(brms::posterior_predict(
      fit$backend_fit,
      newdata = reference_data,
      re_formula = re_formula,
      draw_ids = ids
    )),
    focal = as.matrix(brms::posterior_predict(
      fit$backend_fit,
      newdata = focal_data,
      re_formula = re_formula,
      draw_ids = ids
    ))
  ))
  qfun <- function(row) stats::quantile(row, predictive_quantile, names = FALSE, type = 8)
  q_ref <- apply(pred$reference, 1L, qfun)
  q_focal <- apply(pred$focal, 1L, qfun)
  draws <- data.frame(
    .draw = ids,
    average_log_duration_contrast = log_contrast,
    reference_average_conditional_median = median_ref,
    focal_average_conditional_median = median_focal,
    conditional_median_difference = median_focal - median_ref,
    conditional_median_ratio = median_focal / pmax(median_ref, .Machine$double.eps),
    reference_predictive_quantile = q_ref,
    focal_predictive_quantile = q_focal,
    predictive_quantile_difference = q_focal - q_ref,
    predictive_quantile_ratio = q_focal / pmax(q_ref, .Machine$double.eps),
    stringsAsFactors = FALSE
  )
  .gp3c_estimand_object(
    "duration",
    draws,
    "conditional_median_ratio",
    list(
      condition_column = condition$column,
      reference_level = condition$source_levels[[1L]],
      focal_level = condition$source_levels[[2L]],
      target_rows = nrow(target),
      predictive_quantile = predictive_quantile,
      outcome_unit = fit$outcome_unit,
      include_group_effects = include_group_effects,
      seed = as.integer(seed),
      interpretation = paste(
        "The conditional-median ratio is distinct from an arithmetic-mean ratio.",
        "The predictive quantile includes residual predictive variation."
      )
    )
  )
}

#' Summarise Posterior Estimand Draws
#'
#' @param x A `gp3bayes_estimand` or finite numeric vector.
#' @param quantities Optional estimand-draw columns to summarise.
#' @param probs Three probabilities defining lower, middle, and upper summaries.
#' @return A data frame.
#' @export
summarise_estimand_draws <- function(
  x,
  quantities = NULL,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!is.numeric(probs) || length(probs) != 3L || anyNA(probs) ||
      any(probs < 0 | probs > 1) || is.unsorted(probs, strictly = TRUE)) {
    .gp3c_stop("`probs` must contain three strictly increasing probabilities.")
  }
  if (inherits(x, "gp3bayes_estimand")) {
    draws <- x$draws
    available <- setdiff(names(draws), ".draw")
    if (is.null(quantities)) quantities <- available
    if (!all(quantities %in% available)) {
      .gp3c_stop("Unknown estimand quantities: ",
                 paste(setdiff(quantities, available), collapse = ", "), ".")
    }
    rows <- lapply(quantities, function(quantity) {
      summary <- .gp3c_summary_vector(draws[[quantity]], probs)
      cbind(data.frame(quantity = quantity, stringsAsFactors = FALSE), summary)
    })
    return(do.call(rbind, rows))
  }
  if (is.numeric(x)) {
    return(.gp3c_summary_vector(x, probs))
  }
  .gp3c_stop("`x` must be a gp3bayes estimand or finite numeric vector.")
}

#' @export
print.gp3bayes_estimand <- function(x, ...) {
  cat("\ngp3bayes estimand\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Primary quantity: ", x$primary_quantity, "\n", sep = "")
  summary <- summarise_estimand_draws(x, x$primary_quantity)
  print(summary, row.names = FALSE)
  cat(" Automatic decision: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_estimand <- function(
  x,
  quantity = x$primary_quantity,
  type = c("density", "histogram"),
  ...
) {
  type <- match.arg(type)
  if (!quantity %in% names(x$draws)) .gp3c_stop("Unknown estimand quantity.")
  values <- x$draws[[quantity]]
  if (type == "density") {
    graphics::plot(stats::density(values), main = quantity, xlab = quantity, ...)
    graphics::abline(v = 0, lty = 2)
  } else {
    graphics::hist(values, main = quantity, xlab = quantity, ...)
    graphics::abline(v = 0, lty = 2)
  }
  invisible(x)
}

# -------------------------------------------------------------------------
# Sensitivity and invariance
# -------------------------------------------------------------------------

.gp3c_prior_row <- function(specification, class) {
  table <- specification$priors$table
  rows <- table$parameter_class == class
  if (sum(rows) != 1L) return(NULL)
  table[rows, , drop = FALSE]
}

.gp3c_prior_config <- function(specification) {
  intercept <- .gp3c_prior_row(specification, "Intercept")
  b <- .gp3c_prior_row(specification, "b")
  sd <- .gp3c_prior_row(specification, "sd")
  sigma <- .gp3c_prior_row(specification, "sigma")
  cor <- .gp3c_prior_row(specification, "cor")
  list(
    baseline = specification$priors$baseline,
    intercept_scale = intercept$scale[[1L]],
    coefficient_scale = b$scale[[1L]],
    group_sd_scale = sd$scale[[1L]],
    residual_scale = if (is.null(sigma)) NULL else sigma$scale[[1L]],
    correlation_eta = if (is.null(cor)) 2 else cor$shape[[1L]],
    student_df = sd$df[[1L]],
    advanced = !is.null(specification$advanced_priors),
    main_effect_scale = if (is.null(specification$advanced_priors)) {
      b$scale[[1L]]
    } else specification$advanced_priors$main_effect_scale,
    interaction_scale = if (is.null(specification$advanced_priors)) {
      NULL
    } else specification$advanced_priors$interaction_scale
  )
}

.gp3c_clone_contract <- function(
  contract,
  random_slope = contract$random_slope,
  outcome_unit = contract$outcome_unit
) {
  create_model_contract(
    family = contract$family,
    outcome_col = contract$mappings$outcome,
    participant_col = contract$mappings$participant,
    item_col = contract$mappings$item,
    trial_col = contract$mappings$trial,
    condition_col = contract$mappings$condition,
    time_col = contract$mappings$time,
    predictors = contract$predictors,
    interaction = contract$interaction,
    random_slope = random_slope,
    outcome_unit = outcome_unit,
    notes = contract$notes
  )
}

.gp3c_refresh_prepared <- function(prepared, contract, data = prepared$data) {
  prepared2 <- prepared
  prepared2$data <- data
  prepared2$contract <- contract
  prepared2$audit <- audit_model_readiness(data, contract)
  if (!isTRUE(prepared2$audit$ready)) return(prepared2)
  prepared2$fixed_formula <- .gp3c_fixed_formula(contract)
  prepared2$fixed_formula_text <- paste(deparse(prepared2$fixed_formula), collapse = " ")
  mm <- stats::model.matrix(prepared2$fixed_formula, data = data)
  prepared2$model_matrix_columns <- colnames(mm)
  prepared2$n_analysis_rows <- nrow(data)
  prepared2
}

.gp3c_rebuild_specification <- function(
  prepared,
  template,
  baseline = NULL,
  coefficient_scale = NULL,
  interaction_scale = NULL
) {
  if (!isTRUE(prepared$audit$ready)) {
    .gp3c_stop("Sensitivity specification is not ready after the requested change.")
  }
  cfg <- .gp3c_prior_config(template)
  if (is.null(baseline)) baseline <- cfg$baseline
  if (is.null(coefficient_scale)) coefficient_scale <- cfg$coefficient_scale
  if (identical(template$family, "binary")) {
    if (isTRUE(cfg$advanced) && !is.null(prepared$contract$interaction)) {
      if (is.null(interaction_scale)) interaction_scale <- cfg$interaction_scale
      specify_binary_model_with_interaction_prior(
        prepared = prepared,
        baseline = baseline,
        intercept_scale = cfg$intercept_scale,
        main_effect_scale = coefficient_scale,
        interaction_scale = interaction_scale,
        group_sd_scale = cfg$group_sd_scale,
        correlation_eta = cfg$correlation_eta,
        student_df = cfg$student_df
      )
    } else {
      specify_binary_model(
        prepared = prepared,
        baseline = baseline,
        intercept_scale = cfg$intercept_scale,
        coefficient_scale = coefficient_scale,
        group_sd_scale = cfg$group_sd_scale,
        correlation_eta = cfg$correlation_eta,
        student_df = cfg$student_df
      )
    }
  } else {
    if (isTRUE(cfg$advanced) && !is.null(prepared$contract$interaction)) {
      if (is.null(interaction_scale)) interaction_scale <- cfg$interaction_scale
      specify_duration_model_with_interaction_prior(
        prepared = prepared,
        baseline = baseline,
        intercept_scale = cfg$intercept_scale,
        main_effect_scale = coefficient_scale,
        interaction_scale = interaction_scale,
        group_sd_scale = cfg$group_sd_scale,
        residual_scale = cfg$residual_scale,
        correlation_eta = cfg$correlation_eta,
        student_df = cfg$student_df
      )
    } else {
      specify_duration_model(
        prepared = prepared,
        baseline = baseline,
        intercept_scale = cfg$intercept_scale,
        coefficient_scale = coefficient_scale,
        group_sd_scale = cfg$group_sd_scale,
        residual_scale = cfg$residual_scale,
        correlation_eta = cfg$correlation_eta,
        student_df = cfg$student_df
      )
    }
  }
}

#' Create a Random-Slope Sensitivity Plan
#'
#' Constructs approved random-intercept and random-slope specifications from a
#' common prepared design. No model is fitted and neither structure is selected.
#'
#' @param specification An approved binary or duration specification.
#' @return A `gp3bayes_random_slope_sensitivity_plan`.
#' @export
create_random_slope_sensitivity_plan <- function(specification) {
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3c_stop("`specification` must be a gp3bayes model specification.")
  }
  if (is.null(specification$contract$mappings$condition)) {
    .gp3c_stop("Random-slope sensitivity requires a declared focal condition.")
  }
  make_one <- function(random_slope) {
    contract <- .gp3c_clone_contract(specification$contract, random_slope = random_slope)
    prepared <- .gp3c_refresh_prepared(specification$prepared, contract)
    if (!isTRUE(prepared$audit$ready)) {
      return(list(ready = FALSE, contract = contract, prepared = prepared, specification = NULL))
    }
    rebuilt <- .gp3c_rebuild_specification(prepared, specification)
    list(ready = TRUE, contract = contract, prepared = prepared, specification = rebuilt)
  }
  intercept_only <- make_one(FALSE)
  random_slope <- make_one(TRUE)
  structure(list(
    plan_version = "0.2",
    family = specification$family,
    intercept_only = intercept_only,
    random_slope = random_slope,
    automatic_selection = FALSE,
    interpretation = paste(
      "The plan exposes both approved structures for sensitivity analysis.",
      "Predictive or estimand differences must be interpreted substantively; no structure is selected automatically."
    )
  ), class = "gp3bayes_random_slope_sensitivity_plan")
}

#' Create a Group-Deletion Sensitivity Plan
#'
#' @param specification An approved binary or duration specification.
#' @param group Either participant or item.
#' @param units Optional explicit group levels. If omitted all levels are used
#'   only when their count does not exceed `max_units`.
#' @param max_units Maximum automatic number of omission fits.
#' @return A `gp3bayes_group_deletion_sensitivity_plan`.
#' @export
create_group_deletion_sensitivity_plan <- function(
  specification,
  group = c("participant", "item"),
  units = NULL,
  max_units = 20L
) {
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3c_stop("`specification` must be a gp3bayes model specification.")
  }
  group <- match.arg(group)
  max_units <- .gp3c_integer(max_units, "max_units", 1L)
  group_col <- if (group == "participant") {
    specification$contract$mappings$participant
  } else {
    specification$contract$mappings$item
  }
  if (is.null(group_col)) .gp3c_stop("The requested grouping variable is not declared.")
  available <- unique(as.character(specification$prepared$data[[group_col]]))
  if (is.null(units)) {
    if (length(available) > max_units) {
      .gp3c_stop(
        "The design contains ", length(available), " ", group,
        " levels. Supply `units` explicitly to avoid an unbounded refitting request."
      )
    }
    units <- available
  }
  units <- as.character(units)
  if (anyNA(units) || any(!nzchar(units)) || anyDuplicated(units)) {
    .gp3c_stop("`units` must contain unique non-empty group identifiers.")
  }
  unknown <- setdiff(units, available)
  if (length(unknown)) .gp3c_stop("Unknown omission units: ", paste(unknown, collapse = ", "), ".")
  rows <- lapply(units, function(unit) {
    keep <- as.character(specification$prepared$data[[group_col]]) != unit
    subset_data <- specification$prepared$data[keep, , drop = FALSE]
    audit <- tryCatch(
      audit_model_readiness(subset_data, specification$contract),
      error = function(e) e
    )
    data.frame(
      omitted_unit = unit,
      n_remaining = nrow(subset_data),
      ready = !inherits(audit, "error") && isTRUE(audit$ready),
      status = if (inherits(audit, "error")) "error" else audit$status,
      stringsAsFactors = FALSE
    )
  })
  structure(list(
    plan_version = "0.2",
    family = specification$family,
    group = group,
    group_column = group_col,
    units = units,
    table = do.call(rbind, rows),
    specification = specification,
    max_units = max_units,
    automatic_exclusion = FALSE,
    interpretation = paste(
      "Omission fits are sensitivity analyses only.",
      "No participant or item is automatically excluded because an estimate changes."
    )
  ), class = "gp3bayes_group_deletion_sensitivity_plan")
}

.gp3c_subset_specification <- function(specification, column, omitted_unit) {
  keep <- as.character(specification$prepared$data[[column]]) != omitted_unit
  data <- specification$prepared$data[keep, , drop = FALSE]
  prepared <- .gp3c_refresh_prepared(specification$prepared, specification$contract, data)
  if (!isTRUE(prepared$audit$ready)) {
    .gp3c_stop("Omitting `", omitted_unit, "` produced a design that failed readiness.")
  }
  spec <- specification
  spec$prepared <- prepared
  spec$audit <- prepared$audit
  spec
}

.gp3c_fit_spec <- function(
  specification,
  backend,
  chains,
  iter,
  warmup,
  cores,
  seed,
  adapt_delta,
  max_treedepth,
  refresh
) {
  if (identical(specification$family, "binary")) {
    fit_binary_model_backend(
      specification,
      backend = backend,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      seed = seed,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      refresh = refresh
    )
  } else {
    fit_duration_model_backend(
      specification,
      backend = backend,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      seed = seed,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      refresh = refresh
    )
  }
}

.gp3c_primary_estimand <- function(fit, ndraws, seed) {
  if (identical(fit$family, "binary")) {
    estimate_standardized_probability_contrast(fit, ndraws = ndraws)
  } else {
    estimate_standardized_duration_estimands(fit, ndraws = ndraws, seed = seed)
  }
}

#' Run a Group-Deletion Sensitivity Plan
#'
#' This is an intentionally explicit refitting workflow. It can be expensive.
#' The result reports how the primary estimand changes when declared units are
#' omitted, without automatically labelling any unit invalid.
#'
#' @param plan A group-deletion sensitivity plan.
#' @param backend `"rstan"` or `"cmdstanr"`.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted sampling controls.
#' @param ndraws Optional number of draws used for each estimand.
#' @param retain_fits Whether fitted objects are retained.
#' @return A `gp3bayes_group_deletion_sensitivity`.
#' @export
run_group_deletion_sensitivity <- function(
  plan,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = 1L,
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L,
  ndraws = NULL,
  retain_fits = FALSE
) {
  if (!inherits(plan, "gp3bayes_group_deletion_sensitivity_plan")) {
    .gp3c_stop("`plan` must be a group-deletion sensitivity plan.")
  }
  backend <- match.arg(backend)
  retain_fits <- .gp3c_flag(retain_fits, "retain_fits")
  reference_fit <- .gp3c_fit_spec(
    plan$specification, backend, chains, iter, warmup, cores, seed,
    adapt_delta, max_treedepth, refresh
  )
  reference_estimand <- .gp3c_primary_estimand(reference_fit, ndraws, seed)
  reference_summary <- summarise_estimand_draws(
    reference_estimand, reference_estimand$primary_quantity
  )
  results <- vector("list", length(plan$units))
  fits <- if (retain_fits) vector("list", length(plan$units)) else NULL
  names(results) <- plan$units
  if (retain_fits) names(fits) <- plan$units
  rows <- lapply(seq_along(plan$units), function(i) {
    unit <- plan$units[[i]]
    outcome <- tryCatch({
      spec <- .gp3c_subset_specification(plan$specification, plan$group_column, unit)
      fit <- .gp3c_fit_spec(
        spec, backend, chains, iter, warmup, cores, seed + i,
        adapt_delta, max_treedepth, refresh
      )
      est <- .gp3c_primary_estimand(fit, ndraws, seed + i)
      summary <- summarise_estimand_draws(est, est$primary_quantity)
      results[[i]] <<- est
      if (retain_fits) fits[[i]] <<- fit
      list(summary = summary, error = NULL)
    }, error = function(e) list(summary = NULL, error = conditionMessage(e)))
    if (!is.null(outcome$error)) {
      data.frame(
        omitted_unit = unit,
        status = "error",
        median = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        median_shift = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        omitted_unit = unit,
        status = "completed",
        median = outcome$summary$median,
        lower = outcome$summary$lower,
        upper = outcome$summary$upper,
        median_shift = outcome$summary$median - reference_summary$median,
        stringsAsFactors = FALSE
      )
    }
  })
  structure(list(
    status = "review",
    plan = plan,
    backend = backend,
    reference_estimand = reference_estimand,
    reference_fit = if (retain_fits) reference_fit else NULL,
    results = results,
    fits = fits,
    summary = do.call(rbind, rows),
    automatic_exclusion = FALSE,
    interpretation = paste(
      "Omission sensitivity describes estimand stability under declared deletions.",
      "It does not identify invalid observations automatically."
    )
  ), class = "gp3bayes_group_deletion_sensitivity")
}

#' Run Random-Intercept versus Random-Slope Sensitivity
#'
#' @inheritParams run_group_deletion_sensitivity
#' @param plan A random-slope sensitivity plan.
#' @return A `gp3bayes_random_slope_sensitivity`.
#' @export
run_random_slope_sensitivity <- function(
  plan,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = 1L,
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L,
  ndraws = NULL,
  retain_fits = FALSE
) {
  if (!inherits(plan, "gp3bayes_random_slope_sensitivity_plan")) {
    .gp3c_stop("`plan` must be a random-slope sensitivity plan.")
  }
  if (!isTRUE(plan$intercept_only$ready) || !isTRUE(plan$random_slope$ready)) {
    .gp3c_stop("Both structural specifications must pass readiness before refitting.")
  }
  backend <- match.arg(backend)
  retain_fits <- .gp3c_flag(retain_fits, "retain_fits")
  specs <- list(
    random_intercept = plan$intercept_only$specification,
    random_slope = plan$random_slope$specification
  )
  fits <- lapply(seq_along(specs), function(i) {
    .gp3c_fit_spec(
      specs[[i]], backend, chains, iter, warmup, cores, seed + i - 1L,
      adapt_delta, max_treedepth, refresh
    )
  })
  names(fits) <- names(specs)
  estimands <- lapply(seq_along(fits), function(i) {
    .gp3c_primary_estimand(fits[[i]], ndraws, seed + i - 1L)
  })
  names(estimands) <- names(specs)
  comparison <- compare_estimand_sensitivity(
    estimands[["random_intercept"]],
    list(random_slope = estimands[["random_slope"]])
  )
  structure(list(
    status = "review",
    plan = plan,
    backend = backend,
    estimands = estimands,
    comparison = comparison,
    fits = if (retain_fits) fits else NULL,
    automatic_selection = FALSE,
    interpretation = paste(
      "The comparison quantifies sensitivity to the approved random-slope structure.",
      "No structure is selected automatically."
    )
  ), class = "gp3bayes_random_slope_sensitivity")
}

#' Compare Estimand Sensitivity Across Alternative Fits
#'
#' @param reference Reference `gp3bayes_estimand`.
#' @param alternatives Named list of alternative estimands.
#' @param quantity Quantity to compare; defaults to the reference primary
#'   quantity.
#' @return A `gp3bayes_estimand_sensitivity`.
#' @export
compare_estimand_sensitivity <- function(
  reference,
  alternatives,
  quantity = reference$primary_quantity
) {
  if (!inherits(reference, "gp3bayes_estimand")) {
    .gp3c_stop("`reference` must be a gp3bayes estimand.")
  }
  if (!is.list(alternatives) || length(alternatives) == 0L ||
      is.null(names(alternatives)) || any(!nzchar(names(alternatives)))) {
    .gp3c_stop("`alternatives` must be a non-empty named list of estimands.")
  }
  if (!quantity %in% names(reference$draws)) .gp3c_stop("Unknown reference quantity.")
  ref <- .gp3c_summary_vector(reference$draws[[quantity]])
  rows <- lapply(names(alternatives), function(name) {
    x <- alternatives[[name]]
    if (!inherits(x, "gp3bayes_estimand") || !quantity %in% names(x$draws)) {
      .gp3c_stop("Every alternative must contain the requested estimand quantity.")
    }
    s <- .gp3c_summary_vector(x$draws[[quantity]])
    pooled_sd <- stats::sd(c(reference$draws[[quantity]], x$draws[[quantity]]))
    standardized_shift <- if (is.finite(pooled_sd) && pooled_sd > 0) {
      abs(s$median - ref$median) / pooled_sd
    } else NA_real_
    data.frame(
      alternative = name,
      reference_median = ref$median,
      alternative_median = s$median,
      median_shift = s$median - ref$median,
      standardized_shift = standardized_shift,
      reference_lower = ref$lower,
      reference_upper = ref$upper,
      alternative_lower = s$lower,
      alternative_upper = s$upper,
      stringsAsFactors = FALSE
    )
  })
  structure(list(
    status = "review",
    family = reference$family,
    quantity = quantity,
    reference = reference,
    alternatives = alternatives,
    table = do.call(rbind, rows),
    robustness_established = FALSE,
    interpretation = paste(
      "Sensitivity is reported as posterior-summary shifts.",
      "No universal threshold for robustness is imposed."
    )
  ), class = "gp3bayes_estimand_sensitivity")
}

#' Audit Estimand Invariance Against a Declared Tolerance
#'
#' @param reference,alternative Comparable gp3bayes estimands.
#' @param quantity Quantity to compare.
#' @param tolerance Maximum absolute median difference considered invariant for
#'   the declared scientific use.
#' @return A `gp3bayes_estimand_invariance_audit`.
#' @export
audit_estimand_invariance <- function(
  reference,
  alternative,
  quantity = reference$primary_quantity,
  tolerance
) {
  tolerance <- .gp3c_number(tolerance, "tolerance", 0, Inf)
  comparison <- compare_estimand_sensitivity(
    reference,
    list(alternative = alternative),
    quantity = quantity
  )
  shift <- abs(comparison$table$median_shift[[1L]])
  structure(list(
    status = if (shift <= tolerance) "pass" else "review",
    quantity = quantity,
    absolute_median_shift = shift,
    tolerance = tolerance,
    comparison = comparison,
    invariance_established = shift <= tolerance,
    interpretation = paste(
      "The tolerance is user-declared and quantity-specific.",
      "A pass documents numerical invariance under that tolerance only."
    )
  ), class = "gp3bayes_estimand_invariance_audit")
}

#' Create a Contrast-Coding Sensitivity Specification
#'
#' Replays the prepared data back to the recorded raw scale, applies an
#' alternative two-level condition coding, and rebuilds the approved
#' specification. Because the intercept meaning changes with coding, an
#' explicit new `baseline` is required.
#'
#' @param specification An approved model specification.
#' @param condition_coding Two distinct numeric condition codes.
#' @param baseline Explicit baseline probability or median under the new coding.
#' @return An approved alternative specification.
#' @export
create_contrast_coding_sensitivity_specification <- function(
  specification,
  condition_coding,
  baseline
) {
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3c_stop("`specification` must be a gp3bayes model specification.")
  }
  if (!is.numeric(condition_coding) || length(condition_coding) != 2L ||
      anyNA(condition_coding) || any(!is.finite(condition_coding)) ||
      condition_coding[[1L]] == condition_coding[[2L]]) {
    .gp3c_stop("`condition_coding` must contain two distinct finite numeric values.")
  }
  prepared <- specification$prepared
  recipe <- create_transformation_recipe(prepared)
  raw <- invert_transformation_recipe(prepared$data, recipe)
  condition_levels <- .gp3c_condition_metadata(prepared)$source_levels
  scale_predictors <- if (identical(specification$family, "binary")) {
    names(prepared$transformations$numeric_scaling)
  } else {
    names(prepared$transformations$scaled_columns)
  }
  if (identical(specification$family, "binary")) {
    new_prepared <- prepare_hierarchical_binary_data(
      raw,
      specification$contract,
      outcome_mapping = prepared$transformations$outcome$mapping,
      condition_levels = condition_levels,
      condition_coding = condition_coding,
      scale_predictors = intersect(scale_predictors, specification$contract$predictors),
      scale_time = !is.null(specification$contract$mappings$time) &&
        specification$contract$mappings$time %in% scale_predictors,
      missing = "error"
    )
  } else {
    outcome_transform <- prepared$transformations$outcome
    new_prepared <- prepare_hierarchical_duration_data(
      raw,
      .gp3c_or(prepared$source_contract, specification$contract),
      condition_levels = condition_levels,
      condition_coding = condition_coding,
      scale_predictors = intersect(scale_predictors, specification$contract$predictors),
      scale_time = !is.null(specification$contract$mappings$time) &&
        specification$contract$mappings$time %in% scale_predictors,
      outcome_multiplier = outcome_transform$multiplier,
      converted_unit = if (!identical(outcome_transform$source_unit, outcome_transform$analysis_unit)) {
        outcome_transform$analysis_unit
      } else NULL,
      missing = "error"
    )
  }
  .gp3c_rebuild_specification(new_prepared, specification, baseline = baseline)
}

#' Create a Predictor-Scaling Sensitivity Specification
#'
#' Changes one already-scaled predictor by a declared scale factor and requires
#' an explicit coefficient-prior scale for the new parameterisation. This
#' avoids pretending that a common coefficient prior is automatically invariant
#' to predictor scaling.
#'
#' @param specification An approved model specification.
#' @param predictor A declared predictor that was scaled during preparation.
#' @param scale_factor New scale divided by the original recorded scale. Values
#'   above one make the transformed predictor numerically smaller.
#' @param coefficient_scale Explicit population-coefficient prior scale under
#'   the alternative parameterisation.
#' @param interaction_scale Optional explicit interaction prior scale when the
#'   advanced separate-interaction prior is used.
#' @return An approved alternative specification.
#' @export
create_predictor_scaling_sensitivity_specification <- function(
  specification,
  predictor,
  scale_factor,
  coefficient_scale,
  interaction_scale = NULL
) {
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3c_stop("`specification` must be a gp3bayes model specification.")
  }
  predictor <- .gp3c_string(predictor, "predictor")
  scale_factor <- .gp3c_number(scale_factor, "scale_factor", 0, Inf, TRUE)
  coefficient_scale <- .gp3c_number(
    coefficient_scale, "coefficient_scale", 0, Inf, TRUE
  )
  prepared <- specification$prepared
  if (!predictor %in% specification$contract$predictors) {
    .gp3c_stop("`predictor` must be declared in the approved contract.")
  }
  prepared2 <- prepared
  if (identical(specification$family, "binary")) {
    registry <- prepared$transformations$numeric_scaling
    if (is.null(registry[[predictor]])) {
      .gp3c_stop("The requested predictor was not scaled during binary preparation.")
    }
    prepared2$data[[predictor]] <- prepared$data[[predictor]] / scale_factor
    prepared2$transformations$numeric_scaling[[predictor]][["scale"]] <-
      registry[[predictor]][["scale"]] * scale_factor
  } else {
    registry <- prepared$transformations$scaled_columns
    if (is.null(registry[[predictor]])) {
      .gp3c_stop("The requested predictor was not scaled during duration preparation.")
    }
    prepared2$data[[predictor]] <- prepared$data[[predictor]] / scale_factor
    prepared2$transformations$scaled_columns[[predictor]]$scale <-
      registry[[predictor]]$scale * scale_factor
  }
  prepared2 <- .gp3c_refresh_prepared(prepared2, prepared2$contract, prepared2$data)
  .gp3c_rebuild_specification(
    prepared2,
    specification,
    coefficient_scale = coefficient_scale,
    interaction_scale = interaction_scale
  )
}

#' Create a Duration-Unit Sensitivity Specification
#'
#' Re-expresses an approved prepared duration outcome in a new unit by a
#' positive multiplicative conversion, shifts the baseline median accordingly,
#' and retains all dimensionless prior scales.
#'
#' @param specification An approved duration specification.
#' @param multiplier Positive conversion factor from the current analysis unit
#'   to the new unit.
#' @param new_unit New non-empty unit label.
#' @return An approved duration sensitivity specification.
#' @export
create_duration_unit_sensitivity_specification <- function(
  specification,
  multiplier,
  new_unit
) {
  if (!inherits(specification, "gp3bayes_duration_model_specification")) {
    .gp3c_stop("`specification` must be an approved duration specification.")
  }
  multiplier <- .gp3c_number(multiplier, "multiplier", 0, Inf, TRUE)
  new_unit <- .gp3c_string(new_unit, "new_unit")
  prepared <- specification$prepared
  outcome <- specification$contract$mappings$outcome
  data <- prepared$data
  data[[outcome]] <- data[[outcome]] * multiplier
  contract <- .gp3c_clone_contract(specification$contract, outcome_unit = new_unit)
  prepared2 <- .gp3c_refresh_prepared(prepared, contract, data)
  prepared2$outcome_unit <- new_unit
  prepared2$transformations$outcome$analysis_unit <- new_unit
  prepared2$transformations$outcome$multiplier <-
    prepared$transformations$outcome$multiplier * multiplier
  cfg <- .gp3c_prior_config(specification)
  .gp3c_rebuild_specification(
    prepared2,
    specification,
    baseline = cfg$baseline * multiplier
  )
}

#' Audit Duration-Unit Invariance
#'
#' Checks the unit-free median and predictive-quantile ratios and the expected
#' scaling of absolute predictive quantities after a known unit conversion.
#'
#' @param reference,converted Comparable duration estimands.
#' @param multiplier Conversion factor applied to the outcome unit.
#' @param tolerance Absolute tolerance for unit-free ratios and relative
#'   tolerance for scaled absolute quantities.
#' @return A `gp3bayes_duration_unit_invariance_audit`.
#' @export
audit_duration_unit_invariance <- function(
  reference,
  converted,
  multiplier,
  tolerance = 0.02
) {
  if (!inherits(reference, "gp3bayes_estimand") || reference$family != "duration" ||
      !inherits(converted, "gp3bayes_estimand") || converted$family != "duration") {
    .gp3c_stop("Both estimands must be duration gp3bayes estimands.")
  }
  multiplier <- .gp3c_number(multiplier, "multiplier", 0, Inf, TRUE)
  tolerance <- .gp3c_number(tolerance, "tolerance", 0, Inf)
  ratio_names <- c("conditional_median_ratio", "predictive_quantile_ratio")
  ratio_shift <- vapply(ratio_names, function(name) {
    abs(stats::median(reference$draws[[name]]) - stats::median(converted$draws[[name]]))
  }, numeric(1L))
  absolute_names <- c(
    "reference_average_conditional_median",
    "focal_average_conditional_median",
    "reference_predictive_quantile",
    "focal_predictive_quantile"
  )
  scaling_error <- vapply(absolute_names, function(name) {
    expected <- stats::median(reference$draws[[name]]) * multiplier
    observed <- stats::median(converted$draws[[name]])
    abs(observed - expected) / pmax(abs(expected), .Machine$double.eps)
  }, numeric(1L))
  pass <- all(ratio_shift <= tolerance) && all(scaling_error <= tolerance)
  structure(list(
    status = if (pass) "pass" else "review",
    ratio_shift = ratio_shift,
    relative_scaling_error = scaling_error,
    tolerance = tolerance,
    multiplier = multiplier,
    invariance_established = pass,
    interpretation = paste(
      "Unit-free ratios should remain stable under pure unit conversion,",
      "while absolute duration quantities should scale by the declared multiplier."
    )
  ), class = "gp3bayes_duration_unit_invariance_audit")
}

#' @export
print.gp3bayes_estimand_sensitivity <- function(x, ...) {
  cat("\nEstimand sensitivity\n")
  cat(" Quantity: ", x$quantity, "\n", sep = "")
  print(x$table, row.names = FALSE)
  cat(" Robustness established: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_estimand_sensitivity <- function(x, ...) {
  values <- x$table$alternative_median
  names(values) <- x$table$alternative
  ylim <- range(c(values, x$table$reference_median), finite = TRUE)
  graphics::barplot(values, ylim = ylim, ylab = x$quantity,
                    main = "Alternative estimand medians", ...)
  graphics::abline(h = x$table$reference_median[[1L]], lty = 2)
  invisible(x)
}

# -------------------------------------------------------------------------
# Detailed posterior predictive checks
# -------------------------------------------------------------------------

.gp3c_calibration_table <- function(y, p, bins) {
  bins <- .gp3c_integer(bins, "calibration_bins", 2L)
  breaks <- unique(stats::quantile(p, probs = seq(0, 1, length.out = bins + 1L),
                                   names = FALSE, type = 8))
  if (length(breaks) < 3L) breaks <- seq(0, 1, length.out = bins + 1L)
  breaks[[1L]] <- -Inf
  breaks[[length(breaks)]] <- Inf
  bin <- cut(p, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
  split_index <- split(seq_along(y), bin, drop = TRUE)
  rows <- lapply(names(split_index), function(name) {
    idx <- split_index[[name]]
    data.frame(
      bin = name,
      n = length(idx),
      predicted_probability = mean(p[idx]),
      observed_rate = mean(y[idx]),
      absolute_gap = abs(mean(p[idx]) - mean(y[idx])),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.gp3c_group_rate_table <- function(y, yrep, group, label) {
  if (is.null(group)) return(data.frame())
  split_index <- split(seq_along(y), as.character(group), drop = TRUE)
  rows <- lapply(names(split_index), function(id) {
    idx <- split_index[[id]]
    replicated_rates <- rowMeans(yrep[, idx, drop = FALSE])
    data.frame(
      group = label,
      level = id,
      n = length(idx),
      observed_rate = mean(y[idx]),
      replicated_median_rate = stats::median(replicated_rates),
      replicated_lower = stats::quantile(replicated_rates, 0.025, names = FALSE, type = 8),
      replicated_upper = stats::quantile(replicated_rates, 0.975, names = FALSE, type = 8),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Run Detailed Binary Posterior Predictive Checks
#'
#' Adds calibration bins, participant/item rate checks, focal-condition rates,
#' sparse participant-condition cells, and all-zero/all-one participant
#' patterns to the existing binary PPC workflow.
#'
#' @param fit Approved binary fit.
#' @param draws Number of posterior predictive draws.
#' @param seed Random seed.
#' @param calibration_bins Number of probability calibration bins.
#' @param sparse_cell_min Cell size below which participant-condition cells are
#'   reported as sparse.
#'
#' @return A `gp3bayes_binary_ppc_detail`.
#' @export
check_binary_ppc_details <- function(
  fit,
  draws = 300L,
  seed = 1L,
  calibration_bins = 10L,
  sparse_cell_min = 3L
) {
  .gp3c_fit(fit, "binary")
  .gp3c_require("brms", "run detailed binary posterior predictive checks")
  draws <- .gp3c_integer(draws, "draws", 20L)
  sparse_cell_min <- .gp3c_integer(sparse_cell_min, "sparse_cell_min", 1L)
  data <- fit$specification$prepared$data
  contract <- fit$specification$contract
  y <- as.integer(data[[contract$mappings$outcome]])
  participant <- data[[contract$mappings$participant]]
  item <- if (is.null(contract$mappings$item)) NULL else data[[contract$mappings$item]]
  condition <- if (is.null(contract$mappings$condition)) NULL else data[[contract$mappings$condition]]
  yrep <- .gp3c_with_seed(seed, as.matrix(brms::posterior_predict(
    fit$backend_fit, ndraws = draws
  )))
  ids <- .gp3c_draw_ids(fit$backend_fit, min(draws, nrow(yrep)))
  epred <- as.matrix(brms::posterior_epred(fit$backend_fit, draw_ids = ids))
  p <- colMeans(epred)
  calibration <- .gp3c_calibration_table(y, p, calibration_bins)
  participant_rates <- .gp3c_group_rate_table(y, yrep, participant, "participant")
  item_rates <- .gp3c_group_rate_table(y, yrep, item, "item")
  condition_rates <- .gp3c_group_rate_table(y, yrep, condition, "condition")
  participant_condition <- data.frame()
  sparse_cells <- data.frame()
  if (!is.null(condition)) {
    key <- interaction(participant, condition, drop = TRUE, lex.order = TRUE)
    participant_condition <- .gp3c_group_rate_table(y, yrep, key, "participant_condition")
    sparse_cells <- participant_condition[
      participant_condition$n < sparse_cell_min,
      , drop = FALSE
    ]
  }
  split_participant <- split(seq_along(y), as.character(participant), drop = TRUE)
  observed_degenerate <- mean(vapply(split_participant, function(idx) {
    all(y[idx] == 0) || all(y[idx] == 1)
  }, logical(1L)))
  replicated_degenerate <- apply(yrep, 1L, function(row) {
    mean(vapply(split_participant, function(idx) {
      all(row[idx] == 0) || all(row[idx] == 1)
    }, logical(1L)))
  })
  structure(list(
    check_version = "0.2",
    family = "binary",
    draws = nrow(yrep),
    seed = as.integer(seed),
    observed = y,
    replicated = yrep,
    overall_observed_rate = mean(y),
    replicated_overall_rate = rowMeans(yrep),
    expected_probability = p,
    calibration = calibration,
    maximum_calibration_gap = max(calibration$absolute_gap),
    participant_rates = participant_rates,
    item_rates = item_rates,
    condition_rates = condition_rates,
    participant_condition_rates = participant_condition,
    sparse_cells = sparse_cells,
    sparse_cell_min = sparse_cell_min,
    observed_degenerate_participant_fraction = observed_degenerate,
    replicated_degenerate_participant_fraction = replicated_degenerate,
    groups = list(participant = participant, item = item, condition = condition),
    status = "review",
    adequacy_established = FALSE,
    interpretation = paste(
      "Detailed PPC summaries are deliberately descriptive.",
      "Calibration gaps and sparse cells require scientific review rather than an automatic adequacy label."
    )
  ), class = "gp3bayes_binary_ppc_detail")
}

.gp3c_duration_group_medians <- function(y, yrep, group, label) {
  if (is.null(group)) return(data.frame())
  split_index <- split(seq_along(y), as.character(group), drop = TRUE)
  rows <- lapply(names(split_index), function(id) {
    idx <- split_index[[id]]
    replicated <- apply(yrep[, idx, drop = FALSE], 1L, stats::median)
    data.frame(
      group = label,
      level = id,
      n = length(idx),
      observed_median = stats::median(y[idx]),
      replicated_median = stats::median(replicated),
      replicated_lower = stats::quantile(replicated, 0.025, names = FALSE, type = 8),
      replicated_upper = stats::quantile(replicated, 0.975, names = FALSE, type = 8),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Run Detailed Duration Posterior Predictive Checks
#'
#' Adds raw/log-scale distributions, median and upper-quantile summaries,
#' tail exceedance, group medians, and within-participant focal-condition
#' median ratios.
#'
#' @param fit Approved duration fit.
#' @param draws Number of posterior predictive draws.
#' @param seed Random seed.
#' @param quantiles Predictive quantiles to report.
#' @param tail_threshold Optional substantive tail threshold in the analysis
#'   unit. If omitted, the observed 95th percentile is used descriptively.
#'
#' @return A `gp3bayes_duration_ppc_detail`.
#' @export
check_duration_ppc_details <- function(
  fit,
  draws = 300L,
  seed = 1L,
  quantiles = c(0.5, 0.9, 0.95),
  tail_threshold = NULL
) {
  .gp3c_fit(fit, "duration")
  .gp3c_require("brms", "run detailed duration posterior predictive checks")
  draws <- .gp3c_integer(draws, "draws", 20L)
  if (!is.numeric(quantiles) || length(quantiles) < 1L || anyNA(quantiles) ||
      any(quantiles <= 0 | quantiles >= 1) || is.unsorted(quantiles, strictly = TRUE)) {
    .gp3c_stop("`quantiles` must contain strictly increasing probabilities between zero and one.")
  }
  data <- fit$specification$prepared$data
  contract <- fit$specification$contract
  y <- as.numeric(data[[contract$mappings$outcome]])
  participant <- data[[contract$mappings$participant]]
  item <- if (is.null(contract$mappings$item)) NULL else data[[contract$mappings$item]]
  condition <- if (is.null(contract$mappings$condition)) NULL else data[[contract$mappings$condition]]
  yrep <- .gp3c_with_seed(seed, as.matrix(brms::posterior_predict(
    fit$backend_fit, ndraws = draws
  )))
  if (any(!is.finite(yrep)) || any(yrep <= 0)) {
    .gp3c_stop("Duration posterior predictive draws must be positive and finite.")
  }
  observed_q <- stats::quantile(y, quantiles, names = FALSE, type = 8)
  replicated_q <- sapply(quantiles, function(prob) {
    apply(yrep, 1L, stats::quantile, probs = prob, names = FALSE, type = 8)
  })
  quantile_table <- data.frame(
    probability = quantiles,
    observed = observed_q,
    replicated_median = apply(replicated_q, 2L, stats::median),
    replicated_lower = apply(replicated_q, 2L, stats::quantile, probs = 0.025, names = FALSE, type = 8),
    replicated_upper = apply(replicated_q, 2L, stats::quantile, probs = 0.975, names = FALSE, type = 8),
    stringsAsFactors = FALSE
  )
  threshold_source <- "substantive"
  if (is.null(tail_threshold)) {
    tail_threshold <- stats::quantile(y, 0.95, names = FALSE, type = 8)
    threshold_source <- "observed_q95_descriptive"
  } else {
    tail_threshold <- .gp3c_number(tail_threshold, "tail_threshold", 0, Inf, TRUE)
  }
  observed_tail <- mean(y > tail_threshold)
  replicated_tail <- rowMeans(yrep > tail_threshold)
  participant_medians <- .gp3c_duration_group_medians(y, yrep, participant, "participant")
  item_medians <- .gp3c_duration_group_medians(y, yrep, item, "item")
  condition_medians <- .gp3c_duration_group_medians(y, yrep, condition, "condition")
  within_participant <- data.frame()
  if (!is.null(condition)) {
    levels_condition <- sort(unique(condition))
    if (length(levels_condition) == 2L) {
      split_index <- split(seq_along(y), as.character(participant), drop = TRUE)
      rows <- lapply(names(split_index), function(id) {
        idx <- split_index[[id]]
        cond <- condition[idx]
        if (!all(levels_condition %in% cond)) return(NULL)
        idx_ref <- idx[cond == levels_condition[[1L]]]
        idx_focal <- idx[cond == levels_condition[[2L]]]
        observed_ratio <- stats::median(y[idx_focal]) / stats::median(y[idx_ref])
        replicated_ratio <- apply(yrep, 1L, function(row) {
          stats::median(row[idx_focal]) / stats::median(row[idx_ref])
        })
        data.frame(
          participant = id,
          observed_ratio = observed_ratio,
          replicated_median_ratio = stats::median(replicated_ratio),
          replicated_lower = stats::quantile(replicated_ratio, 0.025, names = FALSE, type = 8),
          replicated_upper = stats::quantile(replicated_ratio, 0.975, names = FALSE, type = 8),
          stringsAsFactors = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) within_participant <- do.call(rbind, rows)
    }
  }
  structure(list(
    check_version = "0.2",
    family = "duration",
    outcome_unit = fit$outcome_unit,
    draws = nrow(yrep),
    seed = as.integer(seed),
    observed = y,
    replicated = yrep,
    observed_median = stats::median(y),
    replicated_median = apply(yrep, 1L, stats::median),
    observed_iqr = stats::IQR(y),
    replicated_iqr = apply(yrep, 1L, stats::IQR),
    quantile_table = quantile_table,
    tail_threshold = tail_threshold,
    tail_threshold_source = threshold_source,
    observed_tail_exceedance = observed_tail,
    replicated_tail_exceedance = replicated_tail,
    participant_medians = participant_medians,
    item_medians = item_medians,
    condition_medians = condition_medians,
    within_participant_condition_ratios = within_participant,
    groups = list(participant = participant, item = item, condition = condition),
    status = "review",
    adequacy_established = FALSE,
    interpretation = paste(
      "Detailed duration PPCs expose tail, group, and within-participant discrepancies.",
      "Systematic failure requests model review; it does not trigger an automatic family switch."
    )
  ), class = "gp3bayes_duration_ppc_detail")
}

#' @export
plot.gp3bayes_binary_ppc_detail <- function(
  x,
  type = c("calibration", "condition", "participant"),
  ...
) {
  type <- match.arg(type)
  if (type == "calibration") {
    tab <- x$calibration
    graphics::plot(
      tab$predicted_probability,
      tab$observed_rate,
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = "Posterior mean predicted probability",
      ylab = "Observed event rate",
      main = "Binary calibration",
      ...
    )
    graphics::abline(0, 1, lty = 2)
  } else if (type == "condition") {
    if (requireNamespace("bayesplot", quietly = TRUE) && !is.null(x$groups$condition)) {
      return(bayesplot::ppc_bars_grouped(x$observed, x$replicated, group = x$groups$condition, ...))
    }
    tab <- x$condition_rates
    if (!nrow(tab)) .gp3c_stop("No condition-rate table is available.")
    graphics::barplot(tab$observed_rate, names.arg = tab$level, ylim = c(0, 1),
                      ylab = "Observed event rate", main = "Condition rates", ...)
  } else {
    tab <- x$participant_rates
    graphics::plot(seq_len(nrow(tab)), tab$observed_rate,
                   ylim = c(0, 1), xlab = "Participant", ylab = "Event rate",
                   main = "Participant event rates", ...)
    graphics::segments(seq_len(nrow(tab)), tab$replicated_lower,
                       seq_len(nrow(tab)), tab$replicated_upper)
    graphics::points(seq_len(nrow(tab)), tab$replicated_median_rate, pch = 1)
  }
  invisible(x)
}

#' @export
plot.gp3bayes_duration_ppc_detail <- function(
  x,
  type = c("ecdf", "log_ecdf", "condition", "tail"),
  ...
) {
  type <- match.arg(type)
  if (type == "ecdf") {
    if (requireNamespace("bayesplot", quietly = TRUE)) {
      return(bayesplot::ppc_ecdf_overlay(x$observed, x$replicated, ...))
    }
    graphics::plot(stats::ecdf(x$observed), main = "Duration ECDF", xlab = x$outcome_unit, ...)
  } else if (type == "log_ecdf") {
    if (requireNamespace("bayesplot", quietly = TRUE)) {
      return(bayesplot::ppc_ecdf_overlay(log(x$observed), log(x$replicated), ...))
    }
    graphics::plot(stats::ecdf(log(x$observed)), main = "Log-duration ECDF", xlab = "Log duration", ...)
  } else if (type == "condition") {
    if (requireNamespace("bayesplot", quietly = TRUE) && !is.null(x$groups$condition)) {
      return(bayesplot::ppc_stat_grouped(
        x$observed, x$replicated, group = x$groups$condition, stat = "median", ...
      ))
    }
    tab <- x$condition_medians
    if (!nrow(tab)) .gp3c_stop("No condition-median table is available.")
    graphics::barplot(tab$observed_median, names.arg = tab$level,
                      ylab = x$outcome_unit, main = "Condition medians", ...)
  } else {
    graphics::hist(
      x$replicated_tail_exceedance,
      xlab = "Replicated tail-exceedance fraction",
      main = "Tail-exceedance PPC",
      ...
    )
    graphics::abline(v = x$observed_tail_exceedance, lty = 2)
  }
  invisible(x)
}

# -------------------------------------------------------------------------
# Exact K-fold adapter and specification traceability
# -------------------------------------------------------------------------

#' Compute Governed Exact K-Fold Cross-Validation
#'
#' Uses `brms::kfold()` as an explicit fallback or complement to PSIS-LOO.
#' Grouped folds may use only the declared participant or item grouping column.
#' No model is selected automatically.
#'
#' @param fit Approved gp3bayes fit.
#' @param K Number of folds for random or stratified splitting.
#' @param folds One of `"random"`, `"stratified"`, or `"grouped"`.
#' @param group Optional declared grouping column used by stratified/grouped
#'   splitting.
#' @param joint One of `"obs"`, `"fold"`, or `"group"`.
#' @param save_fits Whether cross-validation refits are retained.
#' @param seed Random seed.
#' @return A `gp3bayes_kfold_cv`.
#' @export
compute_kfold_cv <- function(
  fit,
  K = 10L,
  folds = c("random", "stratified", "grouped"),
  group = NULL,
  joint = c("obs", "fold", "group"),
  save_fits = FALSE,
  seed = 1L
) {
  .gp3c_fit(fit)
  .gp3c_require("brms", "compute exact K-fold cross-validation")
  K <- .gp3c_integer(K, "K", 2L)
  folds <- match.arg(folds)
  joint <- match.arg(joint)
  save_fits <- .gp3c_flag(save_fits, "save_fits")
  mappings <- fit$specification$contract$mappings
  grouped_allowed <- unname(unlist(
    mappings[c("participant", "item")],
    use.names = FALSE
  ))
  grouped_allowed <- grouped_allowed[
    !is.na(grouped_allowed) & nzchar(grouped_allowed)
  ]
  stratified_allowed <- unname(unlist(
    mappings[c("condition", "participant", "item")],
    use.names = FALSE
  ))
  stratified_allowed <- stratified_allowed[
    !is.na(stratified_allowed) & nzchar(stratified_allowed)
  ]
  if (!is.null(group)) {
    group <- .gp3c_string(group, "group")
    allowed <- if (folds == "grouped") grouped_allowed else stratified_allowed
    if (!group %in% allowed) {
      .gp3c_stop(
        if (folds == "grouped") {
          "Grouped K-fold requires the declared participant or item column."
        } else {
          "`group` must be a declared condition, participant, or item column."
        }
      )
    }
  }
  if (folds == "grouped" && is.null(group)) {
    .gp3c_stop("Grouped K-fold requires the declared participant or item column.")
  }
  folds_arg <- switch(folds, random = NULL, stratified = "stratified", grouped = "grouped")
  raw <- .gp3c_with_seed(seed, brms::kfold(
    fit$backend_fit,
    K = K,
    folds = folds_arg,
    group = group,
    joint = joint,
    compare = FALSE,
    save_fits = save_fits
  ))
  structure(list(
    status = "review",
    raw = raw,
    K = K,
    folds = folds,
    group = group,
    joint = joint,
    seed = as.integer(seed),
    automatic_selection = FALSE,
    interpretation = paste(
      "Exact K-fold cross-validation estimates predictive performance by refitting.",
      "It is not an automatic model-selection rule."
    )
  ), class = "gp3bayes_kfold_cv")
}

#' @export
print.gp3bayes_kfold_cv <- function(x, ...) {
  cat("\nGoverned K-fold cross-validation\n")
  cat(" Folds: ", x$K, "\n", sep = "")
  cat(" Fold strategy: ", x$folds, "\n", sep = "")
  if (!is.null(x$group)) cat(" Group: ", x$group, "\n", sep = "")
  print(x$raw)
  cat(" Automatic model selection: FALSE\n")
  invisible(x)
}

#' Specification-Closure Traceability Matrix
#'
#' Returns an auditable mapping between the Phase-0 closure requirements and
#' their first-class implementation points.
#'
#' @return A data frame.
#' @export
gp3bayes_specification_traceability <- function() {
  data.frame(
    requirement = c(
      "severe overall condition imbalance",
      "participant binary outcome variation",
      "identifier-like numeric predictors",
      "duration extreme-value review",
      "explicit censoring-indicator recognition",
      "declared duration range",
      "fixed-effects separation in strict readiness",
      "random-slope structural sensitivity",
      "participant/item deletion sensitivity",
      "contrast-coding sensitivity specification",
      "predictor-scaling sensitivity specification",
      "duration-unit sensitivity",
      "design-standardised binary probability contrast",
      "duration median ratio and upper predictive quantile",
      "transformation replay on new data",
      "binary detailed PPC calibration/group/cell checks",
      "duration detailed PPC tail/group/within-participant checks",
      "exact K-fold predictive validation adapter"
    ),
    implementation = c(
      "summarise_condition_balance(); audit_model_readiness_strict()",
      "summarise_binary_group_variation(); audit_model_readiness_strict()",
      "identify_identifier_like_predictors(); audit_model_readiness_strict()",
      "review_duration_extremes(); audit_model_readiness_strict()",
      "audit_duration_boundaries(); audit_model_readiness_strict()",
      "audit_duration_boundaries(); audit_model_readiness_strict()",
      "audit_model_readiness_strict(); detect_binary_separation()",
      "create_random_slope_sensitivity_plan(); run_random_slope_sensitivity()",
      "create_group_deletion_sensitivity_plan(); run_group_deletion_sensitivity()",
      "create_contrast_coding_sensitivity_specification(); audit_estimand_invariance()",
      "create_predictor_scaling_sensitivity_specification(); audit_estimand_invariance()",
      "create_duration_unit_sensitivity_specification(); audit_duration_unit_invariance()",
      "estimate_standardized_probability_contrast()",
      "estimate_standardized_duration_estimands()",
      "create_transformation_recipe(); apply_transformation_recipe(); validate_transformation_replay()",
      "check_binary_ppc_details()",
      "check_duration_ppc_details()",
      "compute_kfold_cv()"
    ),
    status = rep("implemented", 18L),
    automatic_decision = rep(FALSE, 18L),
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Convenience print/plot methods for specification-closure audit objects
# -------------------------------------------------------------------------

#' @export
print.gp3bayes_condition_balance <- function(x, ...) {
  cat("\nCondition-balance audit\n")
  cat(" Status: ", x$status, "\n", sep = "")
  if (nrow(x$table)) print(x$table, row.names = FALSE)
  cat(x$interpretation, "\n")
  invisible(x)
}

#' @export
plot.gp3bayes_condition_balance <- function(x, ...) {
  if (!nrow(x$table)) .gp3c_stop("No condition-balance table is available to plot.")
  graphics::barplot(
    x$table$fraction,
    names.arg = x$table$level,
    ylim = c(0, max(0.5, x$table$fraction, na.rm = TRUE)),
    ylab = "Observed fraction",
    main = "Overall condition balance",
    ...
  )
  graphics::abline(h = x$warning_fraction, lty = 2)
  graphics::abline(h = x$failure_fraction, lty = 3)
  invisible(x)
}

#' @export
print.gp3bayes_binary_group_variation <- function(x, ...) {
  cat("\nBinary group-variation audit\n")
  cat(" Group: ", x$group, "\n", sep = "")
  cat(" Status: ", x$status, "\n", sep = "")
  cat(" Groups without outcome variation: ", x$n_no_variation, "\n", sep = "")
  invisible(x)
}

#' @export
plot.gp3bayes_binary_group_variation <- function(x, ...) {
  if (!nrow(x$table)) .gp3c_stop("No group-variation table is available to plot.")
  values <- x$table$n_one / pmax(x$table$n, 1L)
  graphics::barplot(
    values,
    names.arg = x$table$group_id,
    las = 2,
    ylim = c(0, 1),
    ylab = "Observed event rate",
    main = paste("Binary variation by", x$group),
    ...
  )
  invisible(x)
}

#' @export
print.gp3bayes_identifier_predictor_audit <- function(x, ...) {
  cat("\nIdentifier-like predictor audit\n")
  cat(" Status: ", x$status, "\n", sep = "")
  cat(" Flagged predictors: ", if (length(x$flagged)) paste(x$flagged, collapse = ", ") else "none", "\n", sep = "")
  invisible(x)
}

#' @export
plot.gp3bayes_identifier_predictor_audit <- function(x, ...) {
  if (!nrow(x$table)) .gp3c_stop("No predictor-audit table is available to plot.")
  values <- x$table$unique_fraction
  values[!is.finite(values)] <- 0
  graphics::barplot(
    values,
    names.arg = x$table$predictor,
    las = 2,
    ylim = c(0, 1),
    ylab = "Unique fraction",
    main = "Identifier-like predictor review",
    ...
  )
  invisible(x)
}

#' @export
print.gp3bayes_duration_extreme_review <- function(x, ...) {
  cat("\nDuration extreme-value review\n")
  cat(" Status: ", x$status, "\n", sep = "")
  cat(" Flagged rows: ", x$n_flagged, " of ", x$n, "\n", sep = "")
  cat(" Automatic deletion: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_duration_extreme_review <- function(x, ...) {
  graphics::hist(
    x$log_duration,
    xlab = "Log duration",
    main = "Duration extreme-value review",
    ...
  )
  if (nrow(x$flagged)) {
    graphics::rug(x$flagged$log_duration)
  }
  invisible(x)
}

#' @export
print.gp3bayes_duration_boundary_audit <- function(x, ...) {
  cat("\nDuration boundary audit\n")
  cat(" Status: ", x$status, "\n", sep = "")
  print(x$checks, row.names = FALSE)
  cat(" Automatic family switching: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_duration_boundary_audit <- function(x, ...) {
  status_levels <- c("pass", "warn", "fail", "not_applicable")
  counts <- vapply(status_levels, function(level) sum(x$checks$status == level), integer(1L))
  graphics::barplot(
    counts,
    names.arg = status_levels,
    ylab = "Checks",
    main = "Duration boundary audit",
    ...
  )
  invisible(x)
}

#' @export
print.gp3bayes_transformation_recipe <- function(x, ...) {
  cat("\nTransformation recipe\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Fixed formula: ", x$fixed_formula_text, "\n", sep = "")
  if (!is.null(x$outcome_unit)) cat(" Outcome unit: ", x$outcome_unit, "\n", sep = "")
  cat(x$interpretation, "\n")
  invisible(x)
}

#' @export
print.gp3bayes_random_slope_sensitivity_plan <- function(x, ...) {
  cat("\nRandom-slope sensitivity plan\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Intercept-only ready: ", x$intercept_only$ready, "\n", sep = "")
  cat(" Random-slope ready: ", x$random_slope$ready, "\n", sep = "")
  cat(" Automatic selection: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_group_deletion_sensitivity_plan <- function(x, ...) {
  cat("\nGroup-deletion sensitivity plan\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Group: ", x$group, "\n", sep = "")
  cat(" Planned omissions: ", length(x$units), "\n", sep = "")
  cat(" Automatic exclusion: FALSE\n")
  invisible(x)
}

#' Plot Governed Exact K-Fold Results
#'
#' Delegates plotting to the LOO-compatible object returned by `brms::kfold()`.
#'
#' @param x A `gp3bayes_kfold_cv` object.
#' @param ... Arguments passed to the underlying plot method.
#' @return `x`, invisibly.
#' @export
plot.gp3bayes_kfold_cv <- function(x, ...) {
  graphics::plot(x$raw, ...)
  invisible(x)
}

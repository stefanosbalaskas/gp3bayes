# Pre-fit design-support diagnostics for gp3bayes 0.2.0

.gp3dsg_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3dsg_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3dsg_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3dsg_number <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > upper) {
    .gp3dsg_stop("`", name, "` must be one finite value in [", lower, ", ", upper, "].")
  }
  as.numeric(x)
}

.gp3dsg_integer <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < minimum) {
    .gp3dsg_stop("`", name, "` must be one integer >= ", minimum, ".")
  }
  as.integer(x)
}

.gp3dsg_input <- function(x, contract = NULL) {
  if (inherits(x, "gp3bayes_fit")) {
    specification <- x$specification
    return(list(
      data = specification$prepared$data,
      contract = specification$contract,
      prepared = specification$prepared,
      specification = specification
    ))
  }
  if (inherits(x, "gp3bayes_model_specification")) {
    return(list(
      data = x$prepared$data,
      contract = x$contract,
      prepared = x$prepared,
      specification = x
    ))
  }
  if (inherits(x, c("gp3bayes_binary_prepared", "gp3bayes_duration_prepared"))) {
    return(list(data = x$data, contract = x$contract, prepared = x, specification = NULL))
  }
  if (is.data.frame(x)) {
    if (is.null(contract) || !inherits(contract, "gp3bayes_model_contract")) {
      .gp3dsg_stop("A `gp3bayes_model_contract` must be supplied when `x` is a data frame.")
    }
    return(list(data = x, contract = contract, prepared = NULL, specification = NULL))
  }
  .gp3dsg_stop(
    "`x` must be a data frame, prepared gp3bayes object, model specification, or fit."
  )
}

.gp3dsg_status <- function(values) {
  values <- as.character(values)
  if (any(values == "fail", na.rm = TRUE)) return("fail")
  if (any(values %in% c("review", "warn", "not_assessed"), na.rm = TRUE)) return("review")
  "pass"
}

.gp3dsg_fixed_formula <- function(input) {
  if (!is.null(input$prepared$fixed_formula)) return(input$prepared$fixed_formula)
  if (!is.null(input$specification$fixed_formula)) return(input$specification$fixed_formula)
  contract <- input$contract
  if (identical(contract$family, "binary")) {
    return(.gp3b_fixed_formula(contract))
  }
  if (identical(contract$family, "duration")) {
    return(.gp3d_fixed_formula(contract))
  }
  .gp3dsg_stop("Unsupported contract family.")
}

#' Audit Missingness Structure Before Model Fitting
#'
#' Summarises missing values in declared analysis columns and, where possible,
#' by participant, item, and condition. This is a reporting audit; no rows are
#' dropped or imputed.
#'
#' @param x A data frame, prepared object, model specification, or fit.
#' @param contract Required when `x` is a raw data frame.
#' @param review_fraction Column-level missing fraction above which a component
#'   is marked for review.
#' @param fail_fraction Column-level missing fraction above which a component is
#'   marked fail. A fail does not automatically exclude data.
#' @return A `gp3bayes_missingness_audit`.
#' @examples
#' data <- data.frame(
#'   participant_id = rep(c("p1", "p2"), each = 4),
#'   trial_id = rep(1:4, 2),
#'   condition = rep(c("control", "treatment"), 4),
#'   selected = c(0, 1, NA, 1, 1, 0, 1, 0)
#' )
#' contract <- create_model_contract(
#'   "binary", "selected", "participant_id",
#'   trial_col = "trial_id", condition_col = "condition"
#' )
#' audit_missingness_structure(data, contract)
#' @export
audit_missingness_structure <- function(
  x,
  contract = NULL,
  review_fraction = 0.05,
  fail_fraction = 0.20
) {
  review_fraction <- .gp3dsg_number(review_fraction, "review_fraction", 0, 1)
  fail_fraction <- .gp3dsg_number(fail_fraction, "fail_fraction", 0, 1)
  if (review_fraction > fail_fraction) {
    .gp3dsg_stop("`review_fraction` cannot exceed `fail_fraction`.")
  }
  input <- .gp3dsg_input(x, contract)
  data <- input$data
  contract <- input$contract
  columns <- unique(c(
    unlist(contract$mappings, use.names = FALSE),
    contract$predictors,
    contract$interaction
  ))
  columns <- columns[!is.na(columns) & nzchar(columns)]
  available <- intersect(columns, names(data))
  missing_columns <- setdiff(columns, names(data))

  column_table <- if (length(available)) {
    do.call(rbind, lapply(available, function(column) {
      n_missing <- sum(is.na(data[[column]]))
      fraction <- if (nrow(data)) n_missing / nrow(data) else NA_real_
      status <- if (!is.finite(fraction)) {
        "fail"
      } else if (fraction >= fail_fraction && n_missing > 0L) {
        "fail"
      } else if (fraction >= review_fraction && n_missing > 0L) {
        "review"
      } else {
        "pass"
      }
      data.frame(
        column = column,
        n_missing = n_missing,
        fraction_missing = fraction,
        status = status,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame(
      column = character(), n_missing = integer(), fraction_missing = numeric(),
      status = character(), stringsAsFactors = FALSE
    )
  }

  grouping_rows <- list()
  group_map <- contract$mappings[c("participant", "item", "condition")]
  for (group_name in names(group_map)) {
    group_column <- group_map[[group_name]]
    if (is.null(group_column) || !group_column %in% names(data) || !length(available)) next
    missing_any <- !stats::complete.cases(data[available])
    split_flag <- split(missing_any, data[[group_column]], drop = TRUE)
    summary <- do.call(rbind, lapply(names(split_flag), function(level) {
      flags <- split_flag[[level]]
      data.frame(
        group = group_name,
        level = level,
        n_rows = length(flags),
        n_missing_rows = sum(flags),
        fraction_missing_rows = if (length(flags)) mean(flags) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    grouping_rows[[length(grouping_rows) + 1L]] <- summary
  }
  grouping_table <- if (length(grouping_rows)) {
    do.call(rbind, grouping_rows)
  } else {
    data.frame(
      group = character(), level = character(), n_rows = integer(),
      n_missing_rows = integer(), fraction_missing_rows = numeric(),
      stringsAsFactors = FALSE
    )
  }

  overall_status <- if (length(missing_columns)) {
    "fail"
  } else {
    .gp3dsg_status(column_table$status)
  }
  structure(
    list(
      audit_version = "0.2",
      status = overall_status,
      family = contract$family,
      n_rows = nrow(data),
      declared_columns = columns,
      absent_columns = missing_columns,
      column_table = column_table,
      grouping_table = grouping_table,
      review_fraction = review_fraction,
      fail_fraction = fail_fraction,
      automatic_exclusion = FALSE,
      automatic_imputation = FALSE,
      interpretation = paste(
        "Missingness is described but not repaired automatically.",
        "A review/fail status is not an exclusion rule."
      )
    ),
    class = "gp3bayes_missingness_audit"
  )
}

#' Audit the Fixed-Effects Design Matrix
#'
#' Checks rank, singular values, condition number, invariant columns and
#' extreme leverage before fitting Stan. The audit never rewrites a formula or
#' drops a predictor automatically.
#'
#' @inheritParams audit_missingness_structure
#' @param condition_number_review Condition number triggering review.
#' @param condition_number_fail Condition number triggering fail.
#' @param leverage_multiplier Review observations whose hat value is above
#'   `leverage_multiplier * p / n`.
#' @return A `gp3bayes_fixed_effect_design_audit`.
#' @export
audit_fixed_effect_design <- function(
  x,
  contract = NULL,
  condition_number_review = 30,
  condition_number_fail = 100,
  leverage_multiplier = 3
) {
  condition_number_review <- .gp3dsg_number(
    condition_number_review, "condition_number_review", 1, Inf
  )
  condition_number_fail <- .gp3dsg_number(
    condition_number_fail, "condition_number_fail", 1, Inf
  )
  leverage_multiplier <- .gp3dsg_number(
    leverage_multiplier, "leverage_multiplier", 1, Inf
  )
  if (condition_number_review > condition_number_fail) {
    .gp3dsg_stop("`condition_number_review` cannot exceed `condition_number_fail`.")
  }

  input <- .gp3dsg_input(x, contract)
  formula <- .gp3dsg_fixed_formula(input)
  data <- input$data
  model_frame <- tryCatch(
    stats::model.frame(formula, data = data, na.action = stats::na.omit),
    error = function(error) error
  )
  if (inherits(model_frame, "error") || !nrow(model_frame)) {
    detail <- if (inherits(model_frame, "error")) {
      conditionMessage(model_frame)
    } else {
      "No complete rows are available for the declared fixed-effects design."
    }
    return(structure(
      list(
        audit_version = "0.2",
        status = "fail",
        family = input$contract$family,
        formula = formula,
        formula_text = paste(deparse(formula), collapse = " "),
        n_rows = if (inherits(model_frame, "error")) NA_integer_ else 0L,
        n_columns = NA_integer_,
        rank = NA_integer_,
        full_rank = FALSE,
        condition_number = Inf,
        condition_number_review = condition_number_review,
        condition_number_fail = condition_number_fail,
        invariant_columns = character(),
        singular_values = data.frame(
          index = integer(), singular_value = numeric(), stringsAsFactors = FALSE
        ),
        column_table = data.frame(
          column = character(), variance = numeric(), invariant = logical(),
          stringsAsFactors = FALSE
        ),
        leverage = numeric(),
        leverage_threshold = NA_real_,
        high_leverage_rows = integer(),
        high_leverage_data_rows = integer(),
        model_frame_data_rows = integer(),
        error = detail,
        automatic_reparameterization = FALSE,
        automatic_variable_removal = FALSE,
        interpretation = paste(
          "The fixed-effects design could not be constructed for auditing.",
          "The approved model was not altered automatically."
        )
      ),
      class = "gp3bayes_fixed_effect_design_audit"
    ))
  }
  model_frame_rows <- match(rownames(model_frame), rownames(data))
  matrix <- tryCatch(
    stats::model.matrix(formula, data = model_frame),
    error = function(error) error
  )
  if (inherits(matrix, "error")) {
    return(structure(
      list(
        audit_version = "0.2",
        status = "fail",
        family = input$contract$family,
        formula = formula,
        formula_text = paste(deparse(formula), collapse = " "),
        n_rows = nrow(model_frame),
        n_columns = NA_integer_,
        rank = NA_integer_,
        full_rank = FALSE,
        condition_number = Inf,
        condition_number_review = condition_number_review,
        condition_number_fail = condition_number_fail,
        invariant_columns = character(),
        singular_values = data.frame(
          index = integer(), singular_value = numeric(), stringsAsFactors = FALSE
        ),
        column_table = data.frame(
          column = character(), variance = numeric(), invariant = logical(),
          stringsAsFactors = FALSE
        ),
        leverage = numeric(),
        leverage_threshold = NA_real_,
        high_leverage_rows = integer(),
        high_leverage_data_rows = integer(),
        model_frame_data_rows = as.integer(model_frame_rows),
        error = conditionMessage(matrix),
        automatic_reparameterization = FALSE,
        automatic_variable_removal = FALSE,
        interpretation = paste(
          "The fixed-effects model matrix could not be constructed for auditing.",
          "The approved model was not altered automatically."
        )
      ),
      class = "gp3bayes_fixed_effect_design_audit"
    ))
  }
  qr_fit <- qr(matrix)
  rank <- qr_fit$rank
  p <- ncol(matrix)
  n <- nrow(matrix)
  full_rank <- rank == p

  singular_values <- svd(matrix, nu = 0L, nv = 0L)$d
  positive <- singular_values[singular_values > .Machine$double.eps]
  condition_number <- if (length(positive) >= 2L) {
    max(positive) / min(positive)
  } else if (length(positive) == 1L) {
    1
  } else {
    Inf
  }
  variances <- apply(matrix, 2L, stats::var)
  invariant <- names(variances)[is.na(variances) | variances <= .Machine$double.eps]
  invariant <- setdiff(invariant, "(Intercept)")

  leverage <- if (n > 0L && rank > 0L) {
    q_matrix <- qr.Q(qr_fit, complete = FALSE)
    q_rank <- min(rank, ncol(q_matrix))
    rowSums(q_matrix[, seq_len(q_rank), drop = FALSE]^2)
  } else {
    numeric()
  }
  leverage_threshold <- if (n > 0L) leverage_multiplier * max(rank, 1L) / n else NA_real_
  high_leverage <- if (length(leverage)) which(is.finite(leverage) & leverage > leverage_threshold) else integer()

  status <- if (!full_rank || !is.finite(condition_number) ||
                condition_number >= condition_number_fail) {
    "fail"
  } else if (condition_number >= condition_number_review || length(invariant) ||
             length(high_leverage)) {
    "review"
  } else {
    "pass"
  }

  singular_table <- data.frame(
    index = seq_along(singular_values),
    singular_value = singular_values,
    stringsAsFactors = FALSE
  )
  column_table <- data.frame(
    column = colnames(matrix),
    variance = as.numeric(variances),
    invariant = colnames(matrix) %in% invariant,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      audit_version = "0.2",
      status = status,
      family = input$contract$family,
      formula = formula,
      formula_text = paste(deparse(formula), collapse = " "),
      n_rows = n,
      n_columns = p,
      rank = rank,
      full_rank = full_rank,
      condition_number = condition_number,
      condition_number_review = condition_number_review,
      condition_number_fail = condition_number_fail,
      invariant_columns = invariant,
      singular_values = singular_table,
      column_table = column_table,
      leverage = leverage,
      leverage_threshold = leverage_threshold,
      high_leverage_rows = as.integer(high_leverage),
      high_leverage_data_rows = as.integer(model_frame_rows[high_leverage]),
      model_frame_data_rows = as.integer(model_frame_rows),
      error = NULL,
      automatic_reparameterization = FALSE,
      automatic_variable_removal = FALSE,
      interpretation = paste(
        "This is a numerical design-support audit.",
        "It does not automatically alter the approved model specification."
      )
    ),
    class = "gp3bayes_fixed_effect_design_audit"
  )
}

#' Audit Random-Effects Support
#'
#' Audits participant repetition, item crossing, and within-participant
#' condition support for a requested random slope.
#'
#' @inheritParams audit_missingness_structure
#' @param minimum_repeated_rows Minimum observations per participant.
#' @param minimum_group_levels Minimum participant/item levels.
#' @param minimum_condition_cell_rows Minimum rows in each observed
#'   participant-condition cell when a random slope is requested.
#' @return A `gp3bayes_random_effects_support_audit`.
#' @export
audit_random_effects_support <- function(
  x,
  contract = NULL,
  minimum_repeated_rows = 2L,
  minimum_group_levels = 2L,
  minimum_condition_cell_rows = 2L
) {
  minimum_repeated_rows <- .gp3dsg_integer(minimum_repeated_rows, "minimum_repeated_rows")
  minimum_group_levels <- .gp3dsg_integer(minimum_group_levels, "minimum_group_levels")
  minimum_condition_cell_rows <- .gp3dsg_integer(
    minimum_condition_cell_rows, "minimum_condition_cell_rows"
  )
  input <- .gp3dsg_input(x, contract)
  data <- input$data
  contract <- input$contract
  participant_col <- contract$mappings$participant
  item_col <- contract$mappings$item
  condition_col <- contract$mappings$condition

  if (is.null(participant_col) || !participant_col %in% names(data)) {
    component_table <- data.frame(
      component = c("participant_repetition", "item_crossing", "random_slope_support"),
      status = c("fail", "not_assessed", "not_assessed"),
      stringsAsFactors = FALSE
    )
    return(structure(
      list(
        audit_version = "0.2",
        status = "fail",
        family = contract$family,
        random_slope_requested = isTRUE(contract$random_slope),
        component_table = component_table,
        participant_table = data.frame(),
        item_table = data.frame(),
        slope_table = data.frame(),
        error = paste0("Participant column is unavailable: ", participant_col %||% "<NULL>"),
        automatic_simplification = FALSE,
        interpretation = paste(
          "Random-effects support could not be assessed without the declared",
          "participant identifier. No model change was made automatically."
        )
      ),
      class = "gp3bayes_random_effects_support_audit"
    ))
  }

  participant <- data[[participant_col]]
  participant_counts <- table(participant, useNA = "no")
  participant_table <- data.frame(
    participant = names(participant_counts),
    n_rows = as.integer(participant_counts),
    sufficient_rows = as.integer(participant_counts) >= minimum_repeated_rows,
    stringsAsFactors = FALSE
  )
  participant_status <- if (length(participant_counts) < minimum_group_levels ||
                            any(participant_counts < minimum_repeated_rows)) {
    "review"
  } else {
    "pass"
  }

  item_table <- data.frame()
  item_status <- "pass"
  if (!is.null(item_col) && item_col %in% names(data)) {
    item <- data[[item_col]]
    items <- unique(item[!is.na(item)])
    crossing <- vapply(items, function(value) {
      keep <- !is.na(item) & item == value & !is.na(participant)
      length(unique(participant[keep]))
    }, integer(1L))
    item_table <- data.frame(
      item = as.character(items),
      n_participants = crossing,
      crossed = crossing >= minimum_group_levels,
      stringsAsFactors = FALSE
    )
    if (length(items) < minimum_group_levels || any(!item_table$crossed)) {
      item_status <- "review"
    }
  }

  slope_table <- data.frame()
  slope_status <- "pass"
  if (isTRUE(contract$random_slope)) {
    if (is.null(condition_col) || !condition_col %in% names(data)) {
      slope_status <- "fail"
    } else {
      key <- interaction(
        data[[participant_col]], data[[condition_col]],
        drop = TRUE, lex.order = TRUE
      )
      cell_counts <- table(key)
      condition_levels <- tapply(
        data[[condition_col]], data[[participant_col]],
        function(z) length(unique(z[!is.na(z)]))
      )
      slope_table <- data.frame(
        participant = names(condition_levels),
        n_condition_levels = as.integer(condition_levels),
        minimum_cell_rows = vapply(names(condition_levels), function(id) {
          subset_counts <- table(data[[condition_col]][data[[participant_col]] == id])
          if (length(subset_counts)) min(subset_counts) else 0L
        }, integer(1L)),
        stringsAsFactors = FALSE
      )
      insufficient <- slope_table$n_condition_levels < 2L |
        slope_table$minimum_cell_rows < minimum_condition_cell_rows
      if (any(insufficient)) slope_status <- "fail"
      if (!length(cell_counts)) slope_status <- "fail"
    }
  }

  component_table <- data.frame(
    component = c("participant_repetition", "item_crossing", "random_slope_support"),
    status = c(participant_status, item_status, slope_status),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      audit_version = "0.2",
      status = .gp3dsg_status(component_table$status),
      family = contract$family,
      random_slope_requested = isTRUE(contract$random_slope),
      component_table = component_table,
      participant_table = participant_table,
      item_table = item_table,
      slope_table = slope_table,
      error = NULL,
      automatic_simplification = FALSE,
      interpretation = paste(
        "Support is assessed against explicit replication rules.",
        "The audit never simplifies the random-effects structure automatically."
      )
    ),
    class = "gp3bayes_random_effects_support_audit"
  )
}

#' Audit Overall Design Support
#'
#' Combines missingness, fixed-effects design, random-effects support, standard
#' readiness, and optional binary separation screening into one pre-fit audit.
#'
#' @inheritParams audit_missingness_structure
#' @param separation Whether to run [detect_binary_separation()] when the
#'   contract is binary and `detectseparation` is installed.
#' @param strict_readiness Whether to include [audit_model_readiness_strict()].
#' @return A `gp3bayes_design_support_audit`.
#' @export
audit_design_support <- function(
  x,
  contract = NULL,
  separation = FALSE,
  strict_readiness = TRUE
) {
  separation <- .gp3dsg_flag(separation, "separation")
  strict_readiness <- .gp3dsg_flag(strict_readiness, "strict_readiness")
  input <- .gp3dsg_input(x, contract)
  standard <- audit_model_readiness(input$data, input$contract)
  strict <- if (strict_readiness) {
    tryCatch(
      audit_model_readiness_strict(
        input$data, input$contract, run_separation = FALSE
      ),
      error = function(e) e
    )
  } else NULL
  missingness <- audit_missingness_structure(input$data, input$contract)
  fixed <- audit_fixed_effect_design(input$data, input$contract)
  random <- audit_random_effects_support(input$data, input$contract)
  separation_result <- NULL
  if (separation && identical(input$contract$family, "binary")) {
    if (requireNamespace("detectseparation", quietly = TRUE)) {
      separation_result <- tryCatch(
        detect_binary_separation(
          input$specification %||% input$data,
          formula = if (is.null(input$specification)) .gp3dsg_fixed_formula(input) else NULL,
          data = input$data
        ),
        error = function(e) e
      )
    } else {
      separation_result <- structure(
        list(status = "not_assessed", detail = "detectseparation is not installed"),
        class = "gp3bayes_optional_design_component"
      )
    }
  }

  standard_status <- if (!isTRUE(standard$ready)) {
    "fail"
  } else if (identical(standard$status, "ready_with_warnings")) {
    "review"
  } else {
    "pass"
  }
  strict_status <- if (is.null(strict)) {
    "not_assessed"
  } else if (inherits(strict, "error")) {
    "fail"
  } else if (!isTRUE(strict$ready)) {
    "fail"
  } else if (identical(strict$status, "ready_with_warnings")) {
    "review"
  } else {
    "pass"
  }
  separation_status <- if (is.null(separation_result)) {
    "not_assessed"
  } else if (inherits(separation_result, "error")) {
    "review"
  } else {
    separation_result$status %||% "review"
  }
  component_table <- data.frame(
    component = c(
      "standard_readiness", "strict_readiness", "missingness",
      "fixed_effect_design", "random_effects_support", "separation"
    ),
    status = c(
      standard_status,
      strict_status,
      missingness$status,
      fixed$status,
      random$status,
      separation_status
    ),
    stringsAsFactors = FALSE
  )
  assessed_status <- component_table$status[component_table$status != "not_assessed"]
  status <- .gp3dsg_status(assessed_status)
  structure(
    list(
      audit_version = "0.2",
      status = status,
      family = input$contract$family,
      component_table = component_table,
      readiness = standard,
      strict_readiness = strict,
      missingness = missingness,
      fixed_effect_design = fixed,
      random_effects_support = random,
      separation = separation_result,
      automatic_model_change = FALSE,
      automatic_exclusion = FALSE,
      interpretation = paste(
        "This combined audit surfaces design limitations before fitting.",
        "It does not automatically modify the approved analysis."
      )
    ),
    class = "gp3bayes_design_support_audit"
  )
}

#' Preflight an Approved Model Specification
#'
#' Convenience wrapper around [audit_design_support()] for an already-created
#' specification.
#'
#' @param specification A gp3bayes model specification.
#' @param ... Arguments passed to [audit_design_support()].
#' @return A `gp3bayes_design_support_audit`.
#' @export
preflight_model_specification <- function(specification, ...) {
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3dsg_stop("`specification` must inherit from `gp3bayes_model_specification`.")
  }
  audit_design_support(specification, ...)
}

#' @export
print.gp3bayes_missingness_audit <- function(x, ...) {
  cat("<gp3bayes_missingness_audit>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Rows: ", x$n_rows, "\n", sep = "")
  cat("  Missing cells: ", sum(x$column_table$n_missing), "\n", sep = "")
  invisible(x)
}

#' @export
print.gp3bayes_fixed_effect_design_audit <- function(x, ...) {
  cat("<gp3bayes_fixed_effect_design_audit>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Rank: ", x$rank, "/", x$n_columns, "\n", sep = "")
  cat("  Condition number: ", signif(x$condition_number, 5), "\n", sep = "")
  cat("  High-leverage rows: ", length(x$high_leverage_rows), "\n", sep = "")
  invisible(x)
}

#' @export
print.gp3bayes_random_effects_support_audit <- function(x, ...) {
  cat("<gp3bayes_random_effects_support_audit>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  print.data.frame(x$component_table, row.names = FALSE)
  invisible(x)
}

#' @export
print.gp3bayes_design_support_audit <- function(x, ...) {
  cat("<gp3bayes_design_support_audit>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  print.data.frame(x$component_table, row.names = FALSE)
  cat("  Automatic model changes: FALSE\n")
  invisible(x)
}

#' @export
plot.gp3bayes_missingness_audit <- function(x, ...) {
  if (!nrow(x$column_table)) return(invisible(x))
  graphics::barplot(
    x$column_table$fraction_missing,
    names.arg = x$column_table$column,
    las = 2,
    ylab = "Missing fraction",
    ylim = c(0, max(c(x$fail_fraction, x$column_table$fraction_missing), na.rm = TRUE)),
    main = "Declared-column missingness",
    ...
  )
  graphics::abline(h = x$review_fraction, lty = 2)
  graphics::abline(h = x$fail_fraction, lty = 3)
  invisible(x)
}

#' @export
plot.gp3bayes_fixed_effect_design_audit <- function(x, ...) {
  values <- x$singular_values$singular_value
  if (!length(values)) return(invisible(x))
  plot_values <- pmax(values, .Machine$double.eps)
  graphics::plot(
    seq_along(plot_values), plot_values,
    type = "b",
    log = "y",
    xlab = "Singular-value index",
    ylab = "Singular value (log scale)",
    main = "Fixed-effects design singular values",
    ...
  )
  invisible(x)
}

#' @export
plot.gp3bayes_random_effects_support_audit <- function(x, ...) {
  if (!nrow(x$participant_table)) return(invisible(x))
  graphics::barplot(
    x$participant_table$n_rows,
    names.arg = x$participant_table$participant,
    las = 2,
    ylab = "Rows per participant",
    main = "Participant replication",
    ...
  )
  invisible(x)
}

#' @export
plot.gp3bayes_design_support_audit <- function(x, ...) {
  levels <- c("pass", "review", "fail", "not_assessed")
  counts <- table(factor(x$component_table$status, levels = levels))
  graphics::barplot(
    counts,
    names.arg = levels,
    ylab = "Components",
    main = "Pre-fit design-support audit",
    ...
  )
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_missingness_audit <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$column_table, row.names = row.names, optional = optional, ...)
}

#' @export
as.data.frame.gp3bayes_fixed_effect_design_audit <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$column_table, row.names = row.names, optional = optional, ...)
}

#' @export
as.data.frame.gp3bayes_random_effects_support_audit <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$component_table, row.names = row.names, optional = optional, ...)
}

#' @export
as.data.frame.gp3bayes_design_support_audit <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$component_table, row.names = row.names, optional = optional, ...)
}

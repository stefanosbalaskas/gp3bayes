
#' Audit Data Readiness for an Approved Model Contract
#'
#' Audits whether a data frame satisfies the observable data requirements of
#' an existing model contract created by [create_model_contract()]. The audit is
#' backend-independent and does not construct a formula, define executable
#' priors, fit a model, or establish model adequacy.
#'
#' @param data A data frame containing the declared outcome, grouping
#'   identifiers, predictors, and optional design columns.
#' @param contract A `gp3bayes_model_contract` created by
#'   [create_model_contract()].
#'
#' @return An object of class `gp3bayes_readiness_audit`. The object records:
#'
#' * whether the data are ready to proceed to a later model-building gate;
#' * pass, warning, and failure counts;
#' * one structured row per readiness check;
#' * declared and observed column summaries; and
#' * the audited model contract.
#'
#' The input data are not retained in the returned object.
#'
#' @details
#' A readiness audit evaluates observable data properties only. Passing the
#' audit does not establish convergence, model adequacy, predictive validity,
#' causal identification, or substantive validity.
#'
#' Failures block progression to a later model-building gate. Warnings identify
#' weak or unusual structures that require review but do not automatically
#' block progression.
#'
#' Binary outcomes must be logical or numeric values encoded exclusively as
#' zero and one, with both classes observed. Duration outcomes must be numeric,
#' finite, strictly positive, uncensored, and variable.
#'
#' @section Interpretation boundaries:
#' Readiness checks cannot determine whether a model is scientifically
#' justified. Behavioural measurements must not be interpreted as direct
#' measures of latent psychological or protected attributes.
#'
#' @examples
#' binary_data <- data.frame(
#'   participant_id = rep(c("p1", "p2"), each = 4),
#'   trial_id = rep(1:4, times = 2),
#'   condition = rep(c("control", "treatment"), times = 4),
#'   selected = c(0, 1, 0, 1, 1, 0, 1, 0)
#' )
#'
#' binary_contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition"
#' )
#'
#' audit_model_readiness(binary_data, binary_contract)
#'
#' @export
audit_model_readiness <- function(data, contract) {
  .validate_readiness_data(data)
  .validate_readiness_contract(contract)

  checks <- list()

  add_check <- function(
    check_id,
    category,
    status,
    message,
    n_affected = NA_integer_
  ) {
    checks[[length(checks) + 1L]] <<- .readiness_check(
      check_id = check_id,
      category = category,
      status = status,
      message = message,
      n_affected = n_affected
    )

    invisible(NULL)
  }

  mappings <- contract$mappings
  mapped_columns <- unname(unlist(mappings, use.names = FALSE))
  predictor_columns <- contract$predictors
  analysis_columns <- unique(c(mapped_columns, predictor_columns))
  missing_columns <- setdiff(analysis_columns, names(data))
  existing_columns <- intersect(analysis_columns, names(data))

  if (nrow(data) > 0L) {
    add_check(
      check_id = "data_rows",
      category = "data",
      status = "pass",
      message = paste(
        "The data contain",
        nrow(data),
        "rows."
      )
    )
  } else {
    add_check(
      check_id = "data_rows",
      category = "data",
      status = "fail",
      message = "The data contain no rows.",
      n_affected = 0L
    )
  }

  if (length(missing_columns) == 0L) {
    add_check(
      check_id = "required_columns",
      category = "columns",
      status = "pass",
      message = "All declared analysis columns are present."
    )
  } else {
    add_check(
      check_id = "required_columns",
      category = "columns",
      status = "fail",
      message = paste0(
        "Missing declared analysis columns: ",
        .readiness_collapse_names(missing_columns),
        "."
      ),
      n_affected = length(missing_columns)
    )
  }

  .audit_identifier_types(
    data = data,
    mappings = mappings,
    add_check = add_check
  )

  if (length(existing_columns) == 0L) {
    add_check(
      check_id = "analysis_missingness",
      category = "missingness",
      status = "fail",
      message = "No declared analysis columns are available for review.",
      n_affected = nrow(data)
    )
  } else {
    missing_rows <- .readiness_missing_rows(
      data = data,
      columns = existing_columns
    )

    if (any(missing_rows)) {
      add_check(
        check_id = "analysis_missingness",
        category = "missingness",
        status = "fail",
        message = paste(
          sum(missing_rows),
          "rows contain missing values in declared analysis columns."
        ),
        n_affected = sum(missing_rows)
      )
    } else {
      add_check(
        check_id = "analysis_missingness",
        category = "missingness",
        status = "pass",
        message = "Declared analysis columns contain no missing values."
      )
    }
  }

  outcome_col <- mappings$outcome

  if (outcome_col %in% names(data)) {
    if (identical(contract$family, "binary")) {
      .audit_binary_outcome(
        outcome = data[[outcome_col]],
        add_check = add_check
      )
    } else {
      .audit_duration_outcome(
        outcome = data[[outcome_col]],
        add_check = add_check
      )
    }
  }

  .audit_participant_structure(
    data = data,
    participant_col = mappings$participant,
    add_check = add_check
  )

  .audit_item_structure(
    data = data,
    participant_col = mappings$participant,
    item_col = mappings$item,
    add_check = add_check
  )

  .audit_trial_structure(
    data = data,
    participant_col = mappings$participant,
    trial_col = mappings$trial,
    add_check = add_check
  )

  .audit_condition_structure(
    data = data,
    participant_col = mappings$participant,
    condition_col = mappings$condition,
    random_slope = contract$random_slope,
    add_check = add_check
  )

  .audit_time_structure(
    data = data,
    participant_col = mappings$participant,
    time_col = mappings$time,
    add_check = add_check
  )

  .audit_predictor_structure(
    data = data,
    predictors = predictor_columns,
    add_check = add_check
  )

  .audit_interaction_structure(
    data = data,
    contract = contract,
    add_check = add_check
  )

  checks <- do.call(
    rbind,
    checks
  )

  rownames(checks) <- NULL

  status_levels <- c("pass", "warn", "fail")

  status_counts <- stats::setNames(
    vapply(
      status_levels,
      function(status) {
        sum(checks$status == status)
      },
      integer(1)
    ),
    status_levels
  )

  ready <- identical(
    unname(status_counts[["fail"]]),
    0L
  )

  status <- if (!ready) {
    "not_ready"
  } else if (status_counts[["warn"]] > 0L) {
    "ready_with_warnings"
  } else {
    "ready"
  }

  observed <- list(
    participants = .readiness_observed_levels(
      data,
      mappings$participant
    ),
    items = .readiness_observed_levels(
      data,
      mappings$item
    ),
    conditions = .readiness_observed_levels(
      data,
      mappings$condition
    )
  )

  structure(
    list(
      audit_version = "0.1",
      family = contract$family,
      model_family = contract$model_family,
      ready = ready,
      status = status,
      n_rows = nrow(data),
      n_columns = ncol(data),
      status_counts = status_counts,
      checks = checks,
      columns = list(
        mapped = mapped_columns,
        predictors = predictor_columns,
        analysis = analysis_columns,
        missing = missing_columns
      ),
      observed = observed,
      contract = contract
    ),
    class = "gp3bayes_readiness_audit"
  )
}

#' Print a gp3bayes Readiness Audit
#'
#' Prints a concise summary of a model-readiness audit and any warnings or
#' failures. The full structured audit remains available for programmatic
#' inspection.
#'
#' @param x A `gp3bayes_readiness_audit` object.
#' @param ... Additional arguments. They are currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @export
print.gp3bayes_readiness_audit <- function(x, ...) {
  cat("<gp3bayes_readiness_audit>\n")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Rows: ", x$n_rows, "\n", sep = "")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Ready: ", x$ready, "\n", sep = "")
  cat(
    "  Checks: ",
    x$status_counts[["pass"]],
    " passed, ",
    x$status_counts[["warn"]],
    " warnings, ",
    x$status_counts[["fail"]],
    " failures\n",
    sep = ""
  )

  issues <- x$checks[x$checks$status != "pass", , drop = FALSE]

  if (nrow(issues) > 0L) {
    cat("  Issues:\n")

    for (index in seq_len(nrow(issues))) {
      cat(
        "    [",
        toupper(issues$status[[index]]),
        "] ",
        issues$check_id[[index]],
        ": ",
        issues$message[[index]],
        "\n",
        sep = ""
      )
    }
  }

  invisible(x)
}

.validate_readiness_data <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  if (
    is.null(names(data)) ||
    anyNA(names(data)) ||
    any(!nzchar(names(data)))
  ) {
    stop(
      "`data` must have non-empty column names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(names(data))) {
    stop(
      "`data` must not contain duplicated column names.",
      call. = FALSE
    )
  }

  invisible(data)
}

.validate_readiness_contract <- function(contract) {
  if (!inherits(contract, "gp3bayes_model_contract")) {
    stop(
      "`contract` must inherit from `gp3bayes_model_contract`.",
      call. = FALSE
    )
  }

  required_fields <- c(
    "family",
    "model_family",
    "mappings",
    "predictors",
    "interaction",
    "random_slope"
  )

  missing_fields <- setdiff(
    required_fields,
    names(contract)
  )

  if (length(missing_fields) > 0L) {
    stop(
      paste0(
        "`contract` is missing required fields: ",
        .readiness_collapse_names(missing_fields),
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.list(contract$mappings)) {
    stop(
      "`contract$mappings` must be a list.",
      call. = FALSE
    )
  }

  required_mappings <- c(
    "outcome",
    "participant",
    "item",
    "trial",
    "condition",
    "time"
  )

  missing_mappings <- setdiff(
    required_mappings,
    names(contract$mappings)
  )

  if (length(missing_mappings) > 0L) {
    stop(
      paste0(
        "`contract$mappings` is missing: ",
        .readiness_collapse_names(missing_mappings),
        "."
      ),
      call. = FALSE
    )
  }

  .match_contract_family(contract$family)

  invisible(contract)
}

.readiness_check <- function(
  check_id,
  category,
  status,
  message,
  n_affected = NA_integer_
) {
  allowed_statuses <- c(
    "pass",
    "warn",
    "fail"
  )

  if (!status %in% allowed_statuses) {
    stop(
      "Internal error: unsupported readiness-check status.",
      call. = FALSE
    )
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

.readiness_collapse_names <- function(values) {
  paste(
    values,
    collapse = ", "
  )
}

.readiness_plain_vector <- function(value) {
  is.atomic(value) &&
    is.null(dim(value))
}

.readiness_supported_identifier <- function(value) {
  .readiness_plain_vector(value) &&
    (
      is.character(value) ||
      is.factor(value) ||
      is.logical(value) ||
      is.numeric(value)
    )
}

.readiness_supported_predictor <- function(value) {
  .readiness_plain_vector(value) &&
    !is.complex(value) &&
    !is.raw(value) &&
    (
      is.character(value) ||
      is.factor(value) ||
      is.logical(value) ||
      is.numeric(value)
    )
}

.readiness_is_categorical <- function(value) {
  is.character(value) ||
    is.factor(value) ||
    is.logical(value)
}

.readiness_unique_non_missing <- function(value) {
  unique(
    value[!is.na(value)]
  )
}

.readiness_n_unique <- function(value) {
  length(
    .readiness_unique_non_missing(value)
  )
}

.readiness_missing_rows <- function(data, columns) {
  flags <- lapply(
    columns,
    function(column) {
      value <- data[[column]]

      if (
        length(value) != nrow(data) ||
        !is.null(dim(value))
      ) {
        return(
          rep(TRUE, nrow(data))
        )
      }

      is.na(value)
    }
  )

  Reduce(
    function(left, right) {
      left | right
    },
    flags,
    init = rep(FALSE, nrow(data))
  )
}

.readiness_observed_levels <- function(data, column) {
  if (
    is.null(column) ||
    !column %in% names(data)
  ) {
    return(NA_integer_)
  }

  value <- data[[column]]

  if (!.readiness_plain_vector(value)) {
    return(NA_integer_)
  }

  as.integer(
    .readiness_n_unique(value)
  )
}

.audit_identifier_types <- function(
  data,
  mappings,
  add_check
) {
  identifier_columns <- unname(
    unlist(
      mappings[c(
        "participant",
        "item",
        "trial"
      )],
      use.names = FALSE
    )
  )

  identifier_columns <- intersect(
    identifier_columns,
    names(data)
  )

  if (length(identifier_columns) == 0L) {
    add_check(
      check_id = "identifier_types",
      category = "identifiers",
      status = "fail",
      message = "No declared identifier columns are available.",
      n_affected = 0L
    )

    return(invisible(NULL))
  }

  unsupported <- identifier_columns[
    !vapply(
      data[identifier_columns],
      .readiness_supported_identifier,
      logical(1)
    )
  ]

  if (length(unsupported) == 0L) {
    add_check(
      check_id = "identifier_types",
      category = "identifiers",
      status = "pass",
      message = "Declared identifier columns use supported vector types."
    )
  } else {
    add_check(
      check_id = "identifier_types",
      category = "identifiers",
      status = "fail",
      message = paste0(
        "Unsupported identifier column types: ",
        .readiness_collapse_names(unsupported),
        "."
      ),
      n_affected = length(unsupported)
    )
  }

  invisible(NULL)
}

.audit_binary_outcome <- function(
  outcome,
  add_check
) {
  supported_type <- .readiness_plain_vector(outcome) &&
    (
      is.logical(outcome) ||
      is.numeric(outcome)
    )

  if (!supported_type) {
    add_check(
      check_id = "outcome_type",
      category = "outcome",
      status = "fail",
      message = paste(
        "The binary outcome must be a logical or numeric",
        "zero-one vector."
      ),
      n_affected = length(outcome)
    )

    return(invisible(NULL))
  }

  add_check(
    check_id = "outcome_type",
    category = "outcome",
    status = "pass",
    message = "The binary outcome uses a supported vector type."
  )

  numeric_outcome <- as.numeric(outcome)
  observed <- !is.na(numeric_outcome)

  invalid <- observed & (
    !is.finite(numeric_outcome) |
      !numeric_outcome %in% c(0, 1)
  )

  if (any(invalid)) {
    add_check(
      check_id = "outcome_values",
      category = "outcome",
      status = "fail",
      message = paste(
        sum(invalid),
        "binary outcome values are not finite zero-one values."
      ),
      n_affected = sum(invalid)
    )
  } else {
    add_check(
      check_id = "outcome_values",
      category = "outcome",
      status = "pass",
      message = "All observed binary outcome values are zero or one."
    )
  }

  supported_values <- sort(
    unique(
      numeric_outcome[observed & !invalid]
    )
  )

  if (identical(supported_values, c(0, 1))) {
    add_check(
      check_id = "outcome_support",
      category = "outcome",
      status = "pass",
      message = "Both binary outcome classes are observed."
    )
  } else {
    add_check(
      check_id = "outcome_support",
      category = "outcome",
      status = "fail",
      message = paste(
        "Both zero and one must be observed in the binary outcome."
      ),
      n_affected = length(supported_values)
    )
  }

  invisible(NULL)
}

.audit_duration_outcome <- function(
  outcome,
  add_check
) {
  supported_type <- .readiness_plain_vector(outcome) &&
    is.numeric(outcome)

  if (!supported_type) {
    add_check(
      check_id = "outcome_type",
      category = "outcome",
      status = "fail",
      message = "The duration outcome must be a numeric vector.",
      n_affected = length(outcome)
    )

    return(invisible(NULL))
  }

  add_check(
    check_id = "outcome_type",
    category = "outcome",
    status = "pass",
    message = "The duration outcome uses a supported numeric type."
  )

  observed <- !is.na(outcome)
  non_finite <- observed & !is.finite(outcome)

  if (any(non_finite)) {
    add_check(
      check_id = "duration_finite",
      category = "outcome",
      status = "fail",
      message = paste(
        sum(non_finite),
        "observed durations are not finite."
      ),
      n_affected = sum(non_finite)
    )
  } else {
    add_check(
      check_id = "duration_finite",
      category = "outcome",
      status = "pass",
      message = "All observed durations are finite."
    )
  }

  non_positive <- observed &
    is.finite(outcome) &
    outcome <= 0

  if (any(non_positive)) {
    add_check(
      check_id = "duration_positive",
      category = "outcome",
      status = "fail",
      message = paste(
        sum(non_positive),
        "observed durations are not strictly positive."
      ),
      n_affected = sum(non_positive)
    )
  } else {
    add_check(
      check_id = "duration_positive",
      category = "outcome",
      status = "pass",
      message = "All observed durations are strictly positive."
    )
  }

  valid_values <- outcome[
    observed &
      is.finite(outcome) &
      outcome > 0
  ]

  if (length(unique(valid_values)) >= 2L) {
    add_check(
      check_id = "outcome_support",
      category = "outcome",
      status = "pass",
      message = "The duration outcome contains observable variation."
    )
  } else {
    add_check(
      check_id = "outcome_support",
      category = "outcome",
      status = "fail",
      message = "The duration outcome must contain at least two values.",
      n_affected = length(unique(valid_values))
    )
  }

  invisible(NULL)
}

.audit_participant_structure <- function(
  data,
  participant_col,
  add_check
) {
  if (!participant_col %in% names(data)) {
    return(invisible(NULL))
  }

  participant <- data[[participant_col]]

  if (!.readiness_supported_identifier(participant)) {
    return(invisible(NULL))
  }

  n_participants <- .readiness_n_unique(participant)

  if (n_participants >= 2L) {
    add_check(
      check_id = "participant_levels",
      category = "repeated_measures",
      status = "pass",
      message = paste(
        n_participants,
        "participants are observed."
      )
    )
  } else {
    add_check(
      check_id = "participant_levels",
      category = "repeated_measures",
      status = "fail",
      message = "At least two participants must be observed.",
      n_affected = n_participants
    )
  }

  counts <- table(
    participant,
    useNA = "no"
  )

  if (length(counts) == 0L || all(counts <= 1L)) {
    add_check(
      check_id = "repeated_measurement",
      category = "repeated_measures",
      status = "fail",
      message = paste(
        "No participant contributes repeated observations."
      ),
      n_affected = length(counts)
    )
  } else if (any(counts <= 1L)) {
    add_check(
      check_id = "repeated_measurement",
      category = "repeated_measures",
      status = "warn",
      message = paste(
        sum(counts <= 1L),
        "participants contribute only one observation."
      ),
      n_affected = sum(counts <= 1L)
    )
  } else {
    add_check(
      check_id = "repeated_measurement",
      category = "repeated_measures",
      status = "pass",
      message = "Every participant contributes repeated observations."
    )
  }

  invisible(NULL)
}

.audit_item_structure <- function(
  data,
  participant_col,
  item_col,
  add_check
) {
  if (is.null(item_col)) {
    add_check(
      check_id = "item_structure",
      category = "items",
      status = "pass",
      message = "No item identifier was declared."
    )

    return(invisible(NULL))
  }

  if (!item_col %in% names(data)) {
    return(invisible(NULL))
  }

  item <- data[[item_col]]

  if (!.readiness_supported_identifier(item)) {
    return(invisible(NULL))
  }

  n_items <- .readiness_n_unique(item)

  if (n_items >= 2L) {
    add_check(
      check_id = "item_levels",
      category = "items",
      status = "pass",
      message = paste(
        n_items,
        "items are observed."
      )
    )
  } else {
    add_check(
      check_id = "item_levels",
      category = "items",
      status = "fail",
      message = "At least two items must be observed when an item is declared.",
      n_affected = n_items
    )
  }

  if (
    !participant_col %in% names(data) ||
    !.readiness_supported_identifier(data[[participant_col]])
  ) {
    add_check(
      check_id = "item_crossing",
      category = "items",
      status = "fail",
      message = "Item crossing cannot be evaluated without participants."
    )

    return(invisible(NULL))
  }

  participant <- data[[participant_col]]
  complete <- !is.na(participant) & !is.na(item)

  participants_per_item <- tapply(
    as.character(participant[complete]),
    as.character(item[complete]),
    function(value) {
      length(unique(value))
    }
  )

  items_per_participant <- tapply(
    as.character(item[complete]),
    as.character(participant[complete]),
    function(value) {
      length(unique(value))
    }
  )

  weak_items <- sum(participants_per_item < 2L)
  weak_participants <- sum(items_per_participant < 2L)
  weak_total <- weak_items + weak_participants

  if (weak_total == 0L) {
    add_check(
      check_id = "item_crossing",
      category = "items",
      status = "pass",
      message = paste(
        "Items are observed across participants and participants",
        "are observed across items."
      )
    )
  } else {
    add_check(
      check_id = "item_crossing",
      category = "items",
      status = "warn",
      message = paste(
        weak_items,
        "items occur for fewer than two participants and",
        weak_participants,
        "participants occur for fewer than two items."
      ),
      n_affected = weak_total
    )
  }

  invisible(NULL)
}

.audit_trial_structure <- function(
  data,
  participant_col,
  trial_col,
  add_check
) {
  if (is.null(trial_col)) {
    add_check(
      check_id = "trial_key",
      category = "trials",
      status = "pass",
      message = "No trial identifier was declared."
    )

    return(invisible(NULL))
  }

  if (
    !participant_col %in% names(data) ||
    !trial_col %in% names(data)
  ) {
    return(invisible(NULL))
  }

  participant <- data[[participant_col]]
  trial <- data[[trial_col]]

  if (
    !.readiness_supported_identifier(participant) ||
    !.readiness_supported_identifier(trial)
  ) {
    return(invisible(NULL))
  }

  complete <- !is.na(participant) & !is.na(trial)

  key <- paste(
    as.character(participant[complete]),
    as.character(trial[complete]),
    sep = "\r"
  )

  duplicated_key <- duplicated(key) |
    duplicated(key, fromLast = TRUE)

  if (any(duplicated_key)) {
    add_check(
      check_id = "trial_key",
      category = "trials",
      status = "fail",
      message = paste(
        sum(duplicated_key),
        "rows have duplicated participant-trial identifiers."
      ),
      n_affected = sum(duplicated_key)
    )
  } else {
    add_check(
      check_id = "trial_key",
      category = "trials",
      status = "pass",
      message = "Participant-trial identifiers are unique."
    )
  }

  invisible(NULL)
}

.audit_condition_structure <- function(
  data,
  participant_col,
  condition_col,
  random_slope,
  add_check
) {
  if (is.null(condition_col)) {
    add_check(
      check_id = "condition_levels",
      category = "condition",
      status = "pass",
      message = "No focal condition was declared."
    )

    return(invisible(NULL))
  }

  if (!condition_col %in% names(data)) {
    return(invisible(NULL))
  }

  condition <- data[[condition_col]]

  if (!.readiness_supported_predictor(condition)) {
    add_check(
      check_id = "condition_type",
      category = "condition",
      status = "fail",
      message = "The focal condition uses an unsupported column type.",
      n_affected = length(condition)
    )

    return(invisible(NULL))
  }

  add_check(
    check_id = "condition_type",
    category = "condition",
    status = "pass",
    message = "The focal condition uses a supported vector type."
  )

  n_conditions <- .readiness_n_unique(condition)

  if (n_conditions >= 2L) {
    add_check(
      check_id = "condition_levels",
      category = "condition",
      status = "pass",
      message = paste(
        n_conditions,
        "condition levels are observed."
      )
    )
  } else {
    add_check(
      check_id = "condition_levels",
      category = "condition",
      status = "fail",
      message = "At least two condition levels must be observed.",
      n_affected = n_conditions
    )
  }

  if (!random_slope) {
    add_check(
      check_id = "random_slope_support",
      category = "random_effects",
      status = "pass",
      message = "No participant-level random slope was requested."
    )

    return(invisible(NULL))
  }

  if (
    !participant_col %in% names(data) ||
    !.readiness_supported_identifier(data[[participant_col]])
  ) {
    add_check(
      check_id = "random_slope_support",
      category = "random_effects",
      status = "fail",
      message = paste(
        "Random-slope support cannot be evaluated without a",
        "valid participant identifier."
      )
    )

    return(invisible(NULL))
  }

  participant <- data[[participant_col]]
  complete <- !is.na(participant) & !is.na(condition)

  levels_by_participant <- tapply(
    condition[complete],
    participant[complete],
    function(value) {
      length(unique(value))
    }
  )

  insufficient_participants <- sum(
    levels_by_participant < 2L
  )

  if (insufficient_participants == 0L) {
    add_check(
      check_id = "random_slope_support",
      category = "random_effects",
      status = "pass",
      message = paste(
        "Every participant is observed in at least two",
        "condition levels."
      )
    )
  } else {
    add_check(
      check_id = "random_slope_support",
      category = "random_effects",
      status = "fail",
      message = paste(
        insufficient_participants,
        "participants lack within-participant condition variation."
      ),
      n_affected = insufficient_participants
    )
  }

  condition_cells <- interaction(
    as.character(participant[complete]),
    as.character(condition[complete]),
    drop = TRUE,
    lex.order = TRUE
  )

  cell_counts <- table(condition_cells)
  weak_cells <- sum(cell_counts < 2L)

  if (weak_cells == 0L) {
    add_check(
      check_id = "random_slope_replication",
      category = "random_effects",
      status = "pass",
      message = paste(
        "Every observed participant-condition cell contains",
        "at least two rows."
      )
    )
  } else {
    add_check(
      check_id = "random_slope_replication",
      category = "random_effects",
      status = "warn",
      message = paste(
        weak_cells,
        "participant-condition cells contain fewer than two rows."
      ),
      n_affected = weak_cells
    )
  }

  invisible(NULL)
}

.audit_time_structure <- function(
  data,
  participant_col,
  time_col,
  add_check
) {
  if (is.null(time_col)) {
    add_check(
      check_id = "time_structure",
      category = "time",
      status = "pass",
      message = "No linear time or trial-order term was declared."
    )

    return(invisible(NULL))
  }

  if (!time_col %in% names(data)) {
    return(invisible(NULL))
  }

  time <- data[[time_col]]

  if (
    !.readiness_plain_vector(time) ||
    !is.numeric(time)
  ) {
    add_check(
      check_id = "time_type",
      category = "time",
      status = "fail",
      message = "The declared time column must be numeric.",
      n_affected = length(time)
    )

    return(invisible(NULL))
  }

  add_check(
    check_id = "time_type",
    category = "time",
    status = "pass",
    message = "The declared time column is numeric."
  )

  observed <- !is.na(time)
  non_finite <- observed & !is.finite(time)

  if (any(non_finite)) {
    add_check(
      check_id = "time_finite",
      category = "time",
      status = "fail",
      message = paste(
        sum(non_finite),
        "observed time values are not finite."
      ),
      n_affected = sum(non_finite)
    )
  } else {
    add_check(
      check_id = "time_finite",
      category = "time",
      status = "pass",
      message = "All observed time values are finite."
    )
  }

  if (.readiness_n_unique(time[is.finite(time)]) >= 2L) {
    add_check(
      check_id = "time_variation",
      category = "time",
      status = "pass",
      message = "The declared time column contains variation."
    )
  } else {
    add_check(
      check_id = "time_variation",
      category = "time",
      status = "fail",
      message = "The declared time column contains no usable variation."
    )
  }

  if (
    !participant_col %in% names(data) ||
    !.readiness_supported_identifier(data[[participant_col]])
  ) {
    add_check(
      check_id = "time_within_participant",
      category = "time",
      status = "fail",
      message = paste(
        "Within-participant time variation cannot be evaluated",
        "without a valid participant identifier."
      )
    )

    return(invisible(NULL))
  }

  participant <- data[[participant_col]]
  complete <- !is.na(participant) &
    !is.na(time) &
    is.finite(time)

  time_levels <- tapply(
    time[complete],
    participant[complete],
    function(value) {
      length(unique(value))
    }
  )

  no_variation <- sum(time_levels < 2L)

  if (length(time_levels) == 0L || no_variation == length(time_levels)) {
    add_check(
      check_id = "time_within_participant",
      category = "time",
      status = "fail",
      message = "No participant has within-participant time variation.",
      n_affected = no_variation
    )
  } else if (no_variation > 0L) {
    add_check(
      check_id = "time_within_participant",
      category = "time",
      status = "warn",
      message = paste(
        no_variation,
        "participants lack within-participant time variation."
      ),
      n_affected = no_variation
    )
  } else {
    add_check(
      check_id = "time_within_participant",
      category = "time",
      status = "pass",
      message = "Every participant has within-participant time variation."
    )
  }

  invisible(NULL)
}

.audit_predictor_structure <- function(
  data,
  predictors,
  add_check
) {
  if (length(predictors) == 0L) {
    add_check(
      check_id = "predictor_structure",
      category = "predictors",
      status = "pass",
      message = "No additional predictors were declared."
    )

    return(invisible(NULL))
  }

  existing_predictors <- intersect(
    predictors,
    names(data)
  )

  if (length(existing_predictors) == 0L) {
    return(invisible(NULL))
  }

  supported <- vapply(
    data[existing_predictors],
    .readiness_supported_predictor,
    logical(1)
  )

  unsupported <- existing_predictors[!supported]

  if (length(unsupported) > 0L) {
    add_check(
      check_id = "predictor_types",
      category = "predictors",
      status = "fail",
      message = paste0(
        "Unsupported predictor column types: ",
        .readiness_collapse_names(unsupported),
        "."
      ),
      n_affected = length(unsupported)
    )
  } else {
    add_check(
      check_id = "predictor_types",
      category = "predictors",
      status = "pass",
      message = "All declared predictors use supported vector types."
    )
  }

  usable_predictors <- existing_predictors[supported]

  if (length(usable_predictors) == 0L) {
    return(invisible(NULL))
  }

  numeric_predictors <- usable_predictors[
    vapply(
      data[usable_predictors],
      is.numeric,
      logical(1)
    )
  ]

  non_finite_counts <- vapply(
    numeric_predictors,
    function(column) {
      value <- data[[column]]

      sum(
        !is.na(value) &
          !is.finite(value)
      )
    },
    integer(1)
  )

  non_finite_predictors <- names(
    non_finite_counts[non_finite_counts > 0L]
  )

  if (length(non_finite_predictors) > 0L) {
    add_check(
      check_id = "predictor_finite",
      category = "predictors",
      status = "fail",
      message = paste0(
        "Non-finite numeric predictors: ",
        .readiness_collapse_names(non_finite_predictors),
        "."
      ),
      n_affected = sum(
        non_finite_counts[non_finite_counts > 0L]
      )
    )
  } else {
    add_check(
      check_id = "predictor_finite",
      category = "predictors",
      status = "pass",
      message = "Numeric predictors contain only finite observed values."
    )
  }

  variation_counts <- vapply(
    usable_predictors,
    function(column) {
      .readiness_n_unique(
        data[[column]]
      )
    },
    integer(1)
  )

  invariant_predictors <- names(
    variation_counts[variation_counts < 2L]
  )

  if (length(invariant_predictors) > 0L) {
    add_check(
      check_id = "predictor_variation",
      category = "predictors",
      status = "fail",
      message = paste0(
        "Predictors without usable variation: ",
        .readiness_collapse_names(invariant_predictors),
        "."
      ),
      n_affected = length(invariant_predictors)
    )
  } else {
    add_check(
      check_id = "predictor_variation",
      category = "predictors",
      status = "pass",
      message = "All declared predictors contain usable variation."
    )
  }

  text_predictors <- usable_predictors[
    vapply(
      data[usable_predictors],
      function(value) {
        is.character(value) ||
          is.factor(value)
      },
      logical(1)
    )
  ]

  blank_counts <- vapply(
    text_predictors,
    function(column) {
      value <- as.character(
        data[[column]]
      )

      sum(
        !is.na(value) &
          !nzchar(trimws(value))
      )
    },
    integer(1)
  )

  blank_predictors <- names(
    blank_counts[blank_counts > 0L]
  )

  if (length(blank_predictors) > 0L) {
    add_check(
      check_id = "predictor_blanks",
      category = "predictors",
      status = "fail",
      message = paste0(
        "Blank text values occur in predictors: ",
        .readiness_collapse_names(blank_predictors),
        "."
      ),
      n_affected = sum(
        blank_counts[blank_counts > 0L]
      )
    )
  } else {
    add_check(
      check_id = "predictor_blanks",
      category = "predictors",
      status = "pass",
      message = "Text predictors contain no blank observed values."
    )
  }

  factor_predictors <- usable_predictors[
    vapply(
      data[usable_predictors],
      is.factor,
      logical(1)
    )
  ]

  unused_level_counts <- vapply(
    factor_predictors,
    function(column) {
      value <- data[[column]]
      observed_levels <- unique(
        as.character(
          value[!is.na(value)]
        )
      )

      length(
        setdiff(
          levels(value),
          observed_levels
        )
      )
    },
    integer(1)
  )

  unused_factor_predictors <- names(
    unused_level_counts[unused_level_counts > 0L]
  )

  if (length(unused_factor_predictors) > 0L) {
    add_check(
      check_id = "predictor_factor_levels",
      category = "predictors",
      status = "warn",
      message = paste0(
        "Unused factor levels occur in predictors: ",
        .readiness_collapse_names(unused_factor_predictors),
        "."
      ),
      n_affected = sum(
        unused_level_counts[unused_level_counts > 0L]
      )
    )
  } else {
    add_check(
      check_id = "predictor_factor_levels",
      category = "predictors",
      status = "pass",
      message = "Declared factor predictors contain no unused levels."
    )
  }

  invisible(NULL)
}

.audit_interaction_structure <- function(
  data,
  contract,
  add_check
) {
  interaction_terms <- contract$interaction

  if (is.null(interaction_terms)) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "pass",
      message = "No interaction was requested."
    )

    return(invisible(NULL))
  }

  missing_terms <- setdiff(
    interaction_terms,
    names(data)
  )

  if (length(missing_terms) > 0L) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "fail",
      message = paste0(
        "Interaction columns are missing: ",
        .readiness_collapse_names(missing_terms),
        "."
      ),
      n_affected = length(missing_terms)
    )

    return(invisible(NULL))
  }

  supported <- vapply(
    data[interaction_terms],
    .readiness_supported_predictor,
    logical(1)
  )

  if (!all(supported)) {
    unsupported <- interaction_terms[!supported]

    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "fail",
      message = paste0(
        "Unsupported interaction columns: ",
        .readiness_collapse_names(unsupported),
        "."
      ),
      n_affected = length(unsupported)
    )

    return(invisible(NULL))
  }

  variation <- vapply(
    interaction_terms,
    function(column) {
      .readiness_n_unique(
        data[[column]]
      )
    },
    integer(1)
  )

  if (any(variation < 2L)) {
    invariant <- interaction_terms[variation < 2L]

    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "fail",
      message = paste0(
        "Interaction columns without usable variation: ",
        .readiness_collapse_names(invariant),
        "."
      ),
      n_affected = length(invariant)
    )

    return(invisible(NULL))
  }

  both_categorical <- all(
    vapply(
      data[interaction_terms],
      .readiness_is_categorical,
      logical(1)
    )
  )

  if (!both_categorical) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "pass",
      message = "Both declared interaction variables contain variation."
    )

    return(invisible(NULL))
  }

  complete <- stats::complete.cases(
    data[interaction_terms]
  )

  if (!any(complete)) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "fail",
      message = "No complete interaction combinations are observed.",
      n_affected = nrow(data)
    )

    return(invisible(NULL))
  }

  interaction_data <- lapply(
    data[complete, interaction_terms, drop = FALSE],
    as.factor
  )

  combinations <- do.call(
    interaction,
    c(
      interaction_data,
      list(
        drop = TRUE,
        lex.order = TRUE
      )
    )
  )

  combination_counts <- table(combinations)

  if (length(combination_counts) < 2L) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "fail",
      message = "Fewer than two interaction combinations are observed.",
      n_affected = length(combination_counts)
    )
  } else if (any(combination_counts < 2L)) {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "warn",
      message = paste(
        sum(combination_counts < 2L),
        "categorical interaction combinations contain one row."
      ),
      n_affected = sum(combination_counts < 2L)
    )
  } else {
    add_check(
      check_id = "interaction_support",
      category = "interaction",
      status = "pass",
      message = "Categorical interaction combinations are replicated."
    )
  }

  invisible(NULL)
}

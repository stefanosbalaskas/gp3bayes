
#' Build an Approved Model Formula
#'
#' Constructs a backend-independent R formula from a
#' [create_model_contract()] result. The formula records the approved fixed
#' effects, one optional interaction, the participant grouping structure, an
#' optional participant-level random slope, and an optional crossed item
#' intercept.
#'
#' @param contract A `gp3bayes_model_contract` created by
#'   [create_model_contract()].
#'
#' @return An R formula. The formula is a specification only and has not been
#'   translated to, validated by, or fitted with a Bayesian backend.
#'
#' @details
#' The participant random intercept is always included. When
#' `contract$random_slope` is `TRUE`, it is replaced by a correlated
#' participant intercept-and-condition-slope term. A declared item identifier
#' adds a crossed item random intercept.
#'
#' The trial identifier is treated as a row key and is not added as a
#' predictor or grouping factor. A declared time column is included as a
#' linear population-level term only.
#'
#' @examples
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "stimulus_id",
#'   condition_col = "condition",
#'   predictors = "age_z"
#' )
#'
#' build_model_formula(contract)
#'
#' @export
build_model_formula <- function(contract) {
  .validate_specification_contract(contract)

  mappings <- contract$mappings

  fixed_terms <- unique(
    c(
      mappings$condition,
      mappings$time,
      contract$predictors
    )
  )

  fixed_terms <- fixed_terms[
    !is.na(fixed_terms) &
      nzchar(fixed_terms)
  ]

  quoted_fixed_terms <- vapply(
    fixed_terms,
    .quote_formula_name,
    character(1)
  )

  interaction_term <- character()

  if (!is.null(contract$interaction)) {
    interaction_term <- paste(
      vapply(
        contract$interaction,
        .quote_formula_name,
        character(1)
      ),
      collapse = ":"
    )
  }

  participant <- .quote_formula_name(
    mappings$participant
  )

  participant_term <- if (contract$random_slope) {
    condition <- .quote_formula_name(
      mappings$condition
    )

    paste0(
      "(1 + ",
      condition,
      " | ",
      participant,
      ")"
    )
  } else {
    paste0(
      "(1 | ",
      participant,
      ")"
    )
  }

  item_term <- character()

  if (!is.null(mappings$item)) {
    item_term <- paste0(
      "(1 | ",
      .quote_formula_name(mappings$item),
      ")"
    )
  }

  right_hand_side <- c(
    quoted_fixed_terms,
    interaction_term,
    participant_term,
    item_term
  )

  right_hand_side <- right_hand_side[
    nzchar(right_hand_side)
  ]

  formula_text <- paste(
    .quote_formula_name(mappings$outcome),
    "~",
    paste(
      right_hand_side,
      collapse = " + "
    )
  )

  stats::as.formula(
    formula_text,
    env = baseenv()
  )
}

#' Create a Backend-Independent Prior Specification
#'
#' Creates an inspectable prior table for one approved model family. The
#' returned object contains no backend-specific prior objects and performs no
#' sampling.
#'
#' @param contract A `gp3bayes_model_contract` created by
#'   [create_model_contract()].
#' @param baseline Numeric scalar describing the expected baseline outcome.
#'   For binary models this is a probability strictly between zero and one.
#'   The default is `0.5`. For duration models this is a strictly positive
#'   baseline median in the recorded outcome unit and must be supplied.
#' @param intercept_scale Optional positive numeric scalar for the normal
#'   intercept prior. Defaults to `1.5` for binary models and `1` for duration
#'   models.
#' @param coefficient_scale Optional positive numeric scalar for normal
#'   population-level coefficient priors. Defaults to `1` for binary models
#'   and `0.5` for duration models.
#' @param group_sd_scale Positive numeric scalar for half-Student-t
#'   group-level standard-deviation priors.
#' @param residual_scale Optional positive numeric scalar for the
#'   half-Student-t residual standard-deviation prior. It applies only to the
#'   duration family and defaults to `1`.
#' @param correlation_eta Numeric scalar greater than or equal to one for the
#'   LKJ prior used when a participant-level random slope is requested.
#' @param student_df Positive numeric scalar giving the degrees of freedom for
#'   half-Student-t scale priors.
#'
#' @return An object of class `gp3bayes_prior_specification`.
#'
#' @details
#' Binary baseline probabilities are transformed with the logit function.
#' Duration baseline medians are transformed with the natural logarithm.
#'
#' @examples
#' binary_contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id"
#' )
#'
#' create_prior_specification(
#'   binary_contract,
#'   baseline = 0.35
#' )
#'
#' @export
create_prior_specification <- function(
  contract,
  baseline = NULL,
  intercept_scale = NULL,
  coefficient_scale = NULL,
  group_sd_scale = 1,
  residual_scale = NULL,
  correlation_eta = 2,
  student_df = 3
) {
  .validate_specification_contract(contract)

  family <- contract$family

  if (is.null(baseline)) {
    if (identical(family, "binary")) {
      baseline <- 0.5
    } else {
      stop(
        paste(
          "`baseline` must be supplied for the duration family",
          "in the recorded outcome unit."
        ),
        call. = FALSE
      )
    }
  }

  if (identical(family, "binary")) {
    baseline <- .validate_probability_scalar(
      baseline,
      "baseline"
    )

    transformed_baseline <- stats::qlogis(
      baseline
    )

    if (is.null(intercept_scale)) {
      intercept_scale <- 1.5
    }

    if (is.null(coefficient_scale)) {
      coefficient_scale <- 1
    }

    if (!is.null(residual_scale)) {
      stop(
        paste(
          "`residual_scale` must be NULL for the binary family",
          "because Bernoulli residual variation is not estimated",
          "as a separate parameter."
        ),
        call. = FALSE
      )
    }
  } else {
    baseline <- .validate_positive_scalar(
      baseline,
      "baseline"
    )

    transformed_baseline <- log(
      baseline
    )

    if (is.null(intercept_scale)) {
      intercept_scale <- 1
    }

    if (is.null(coefficient_scale)) {
      coefficient_scale <- 0.5
    }

    if (is.null(residual_scale)) {
      residual_scale <- 1
    }
  }

  intercept_scale <- .validate_positive_scalar(
    intercept_scale,
    "intercept_scale"
  )

  coefficient_scale <- .validate_positive_scalar(
    coefficient_scale,
    "coefficient_scale"
  )

  group_sd_scale <- .validate_positive_scalar(
    group_sd_scale,
    "group_sd_scale"
  )

  correlation_eta <- .validate_numeric_scalar(
    correlation_eta,
    "correlation_eta"
  )

  if (correlation_eta < 1) {
    stop(
      "`correlation_eta` must be greater than or equal to one.",
      call. = FALSE
    )
  }

  student_df <- .validate_positive_scalar(
    student_df,
    "student_df"
  )

  if (identical(family, "duration")) {
    residual_scale <- .validate_positive_scalar(
      residual_scale,
      "residual_scale"
    )
  }

  prior_rows <- list(
    .prior_row(
      parameter_class = "Intercept",
      distribution = "normal",
      target = "Population-level intercept",
      location = transformed_baseline,
      scale = intercept_scale,
      lower = -Inf,
      upper = Inf,
      rationale = contract$prior_rationale[[1L]]
    ),
    .prior_row(
      parameter_class = "b",
      distribution = "normal",
      target = "Population-level coefficients",
      location = 0,
      scale = coefficient_scale,
      lower = -Inf,
      upper = Inf,
      rationale = contract$prior_rationale[[2L]]
    ),
    .prior_row(
      parameter_class = "sd",
      distribution = "student_t",
      target = "Group-level standard deviations",
      location = 0,
      scale = group_sd_scale,
      df = student_df,
      lower = 0,
      upper = Inf,
      rationale = contract$prior_rationale[[3L]]
    )
  )

  if (identical(family, "duration")) {
    prior_rows[[length(prior_rows) + 1L]] <- .prior_row(
      parameter_class = "sigma",
      distribution = "student_t",
      target = "Residual standard deviation",
      location = 0,
      scale = residual_scale,
      df = student_df,
      lower = 0,
      upper = Inf,
      rationale = contract$prior_rationale[[3L]]
    )
  }

  if (contract$random_slope) {
    prior_rows[[length(prior_rows) + 1L]] <- .prior_row(
      parameter_class = "cor",
      distribution = "lkj",
      target = "Participant intercept-slope correlation",
      shape = correlation_eta,
      lower = -1,
      upper = 1,
      rationale = contract$prior_rationale[[4L]]
    )
  }

  prior_table <- do.call(
    rbind,
    prior_rows
  )

  rownames(prior_table) <- NULL

  priors <- structure(
    list(
      prior_version = "0.1",
      family = family,
      model_family = contract$model_family,
      outcome_unit = contract$outcome_unit,
      random_slope = contract$random_slope,
      baseline = baseline,
      transformed_baseline = transformed_baseline,
      table = prior_table,
      backend = "none",
      executable = FALSE
    ),
    class = "gp3bayes_prior_specification"
  )

  validate_prior_specification(
    priors,
    contract = contract
  )

  priors
}

#' Validate a Prior Specification
#'
#' Validates the completeness and internal consistency of a
#' `gp3bayes_prior_specification`.
#'
#' @param priors A `gp3bayes_prior_specification` created by
#'   [create_prior_specification()].
#' @param contract Optional `gp3bayes_model_contract`.
#'
#' @return `priors`, invisibly.
#'
#' @export
validate_prior_specification <- function(
  priors,
  contract = NULL
) {
  if (!inherits(priors, "gp3bayes_prior_specification")) {
    stop(
      paste(
        "`priors` must inherit from",
        "`gp3bayes_prior_specification`."
      ),
      call. = FALSE
    )
  }

  required_fields <- c(
    "prior_version",
    "family",
    "model_family",
    "outcome_unit",
    "random_slope",
    "baseline",
    "transformed_baseline",
    "table",
    "backend",
    "executable"
  )

  missing_fields <- setdiff(
    required_fields,
    names(priors)
  )

  if (length(missing_fields) > 0L) {
    stop(
      paste0(
        "`priors` is missing required fields: ",
        paste(missing_fields, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  .match_contract_family(
    priors$family
  )

  if (
    !is.logical(priors$random_slope) ||
    length(priors$random_slope) != 1L ||
    is.na(priors$random_slope)
  ) {
    stop(
      "`priors$random_slope` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!identical(priors$backend, "none")) {
    stop(
      "`priors$backend` must be \"none\".",
      call. = FALSE
    )
  }

  if (!identical(priors$executable, FALSE)) {
    stop(
      "`priors$executable` must be FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(contract)) {
    .validate_specification_contract(contract)

    compatibility <- c(
      family = identical(
        priors$family,
        contract$family
      ),
      model_family = identical(
        priors$model_family,
        contract$model_family
      ),
      outcome_unit = identical(
        priors$outcome_unit,
        contract$outcome_unit
      ),
      random_slope = identical(
        priors$random_slope,
        contract$random_slope
      )
    )

    if (!all(compatibility)) {
      incompatible <- names(
        compatibility[!compatibility]
      )

      stop(
        paste0(
          "`priors` is incompatible with `contract`: ",
          paste(incompatible, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  }

  if (identical(priors$family, "binary")) {
    baseline <- .validate_probability_scalar(
      priors$baseline,
      "priors$baseline"
    )

    expected_transformed_baseline <- stats::qlogis(
      baseline
    )
  } else {
    baseline <- .validate_positive_scalar(
      priors$baseline,
      "priors$baseline"
    )

    expected_transformed_baseline <- log(
      baseline
    )
  }

  transformed_baseline <- .validate_numeric_scalar(
    priors$transformed_baseline,
    "priors$transformed_baseline"
  )

  if (
    !isTRUE(
      all.equal(
        transformed_baseline,
        expected_transformed_baseline,
        tolerance = 1e-12
      )
    )
  ) {
    stop(
      paste(
        "`priors$transformed_baseline` is inconsistent",
        "with `priors$baseline` and the model family."
      ),
      call. = FALSE
    )
  }

  prior_table <- priors$table

  if (!is.data.frame(prior_table)) {
    stop(
      "`priors$table` must be a data frame.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "parameter_class",
    "distribution",
    "target",
    "location",
    "scale",
    "df",
    "shape",
    "lower",
    "upper",
    "rationale"
  )

  missing_columns <- setdiff(
    required_columns,
    names(prior_table)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "`priors$table` is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(prior_table) == 0L) {
    stop(
      "`priors$table` must contain at least one prior row.",
      call. = FALSE
    )
  }

  if (anyDuplicated(prior_table$parameter_class)) {
    stop(
      "Prior parameter classes must be unique.",
      call. = FALSE
    )
  }

  expected_classes <- c(
    "Intercept",
    "b",
    "sd"
  )

  if (identical(priors$family, "duration")) {
    expected_classes <- c(
      expected_classes,
      "sigma"
    )
  }

  if (priors$random_slope) {
    expected_classes <- c(
      expected_classes,
      "cor"
    )
  }

  missing_classes <- setdiff(
    expected_classes,
    prior_table$parameter_class
  )

  unsupported_classes <- setdiff(
    prior_table$parameter_class,
    expected_classes
  )

  if (
    length(missing_classes) > 0L ||
    length(unsupported_classes) > 0L
  ) {
    details <- character()

    if (length(missing_classes) > 0L) {
      details <- c(
        details,
        paste0(
          "missing: ",
          paste(missing_classes, collapse = ", ")
        )
      )
    }

    if (length(unsupported_classes) > 0L) {
      details <- c(
        details,
        paste0(
          "unsupported: ",
          paste(unsupported_classes, collapse = ", ")
        )
      )
    }

    stop(
      paste0(
        "Prior parameter classes are incomplete or unsupported (",
        paste(details, collapse = "; "),
        ")."
      ),
      call. = FALSE
    )
  }

  expected_distributions <- c(
    Intercept = "normal",
    b = "normal",
    sd = "student_t",
    sigma = "student_t",
    cor = "lkj"
  )

  observed_distributions <- stats::setNames(
    prior_table$distribution,
    prior_table$parameter_class
  )

  incorrect_distributions <- expected_classes[
    observed_distributions[expected_classes] !=
      expected_distributions[expected_classes]
  ]

  if (length(incorrect_distributions) > 0L) {
    stop(
      paste0(
        "Incorrect prior distributions for: ",
        paste(incorrect_distributions, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    anyNA(prior_table$target) ||
    any(!nzchar(prior_table$target))
  ) {
    stop(
      "Every prior row must contain a non-empty target.",
      call. = FALSE
    )
  }

  if (
    anyNA(prior_table$rationale) ||
    any(!nzchar(prior_table$rationale))
  ) {
    stop(
      "Every prior row must contain a non-empty rationale.",
      call. = FALSE
    )
  }

  normal_rows <- prior_table$distribution == "normal"

  if (
    any(!is.finite(prior_table$location[normal_rows])) ||
    any(!is.finite(prior_table$scale[normal_rows])) ||
    any(prior_table$scale[normal_rows] <= 0)
  ) {
    stop(
      paste(
        "Normal priors require finite locations and",
        "strictly positive finite scales."
      ),
      call. = FALSE
    )
  }

  student_rows <- prior_table$distribution == "student_t"

  if (
    any(!is.finite(prior_table$location[student_rows])) ||
    any(prior_table$location[student_rows] != 0) ||
    any(!is.finite(prior_table$scale[student_rows])) ||
    any(prior_table$scale[student_rows] <= 0) ||
    any(!is.finite(prior_table$df[student_rows])) ||
    any(prior_table$df[student_rows] <= 0) ||
    any(prior_table$lower[student_rows] != 0)
  ) {
    stop(
      paste(
        "Half-Student-t priors require zero locations,",
        "positive scales and degrees of freedom, and lower bounds of zero."
      ),
      call. = FALSE
    )
  }

  lkj_rows <- prior_table$distribution == "lkj"

  if (any(lkj_rows)) {
    if (
      any(!is.finite(prior_table$shape[lkj_rows])) ||
      any(prior_table$shape[lkj_rows] < 1) ||
      any(prior_table$lower[lkj_rows] != -1) ||
      any(prior_table$upper[lkj_rows] != 1)
    ) {
      stop(
        paste(
          "LKJ priors require finite shape values of at least one",
          "and correlation bounds from -1 to 1."
        ),
        call. = FALSE
      )
    }
  }

  invisible(priors)
}

#' Create a Complete Model Specification
#'
#' Combines a model contract, a successful readiness audit, an approved
#' formula, and a validated prior specification into one backend-independent
#' model specification.
#'
#' @param contract A `gp3bayes_model_contract`.
#' @param audit A `gp3bayes_readiness_audit`.
#' @param priors A `gp3bayes_prior_specification`.
#'
#' @return An object of class `gp3bayes_model_specification`.
#'
#' @export
create_model_specification <- function(
  contract,
  audit,
  priors
) {
  .validate_specification_contract(contract)
  .validate_specification_audit(audit)

  if (!identical(audit$contract, contract)) {
    stop(
      paste(
        "`audit$contract` must be identical to the supplied",
        "`contract`."
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(audit$ready)) {
    stop(
      paste0(
        "`audit` is not ready for model specification. Status: ",
        audit$status,
        "."
      ),
      call. = FALSE
    )
  }

  validate_prior_specification(
    priors,
    contract = contract
  )

  model_formula <- build_model_formula(
    contract
  )

  structure(
    list(
      specification_version = "0.1",
      family = contract$family,
      model_family = contract$model_family,
      formula = model_formula,
      formula_text = .formula_to_text(model_formula),
      readiness_status = audit$status,
      warning_count = unname(
        audit$status_counts[["warn"]]
      ),
      contract = contract,
      audit = audit,
      priors = priors,
      backend = "none",
      fit_performed = FALSE
    ),
    class = "gp3bayes_model_specification"
  )
}

#' Print a gp3bayes Prior Specification
#'
#' @param x A `gp3bayes_prior_specification` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @export
print.gp3bayes_prior_specification <- function(x, ...) {
  cat("<gp3bayes_prior_specification>\n")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Baseline: ", x$baseline, "\n", sep = "")

  if (!is.null(x$outcome_unit)) {
    cat(
      "  Outcome unit: ",
      x$outcome_unit,
      "\n",
      sep = ""
    )
  }

  cat(
    "  Parameter classes: ",
    paste(
      x$table$parameter_class,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  cat("  Backend: none\n")
  cat("  Executable: FALSE\n")

  invisible(x)
}

#' Print a gp3bayes Model Specification
#'
#' @param x A `gp3bayes_model_specification` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @export
print.gp3bayes_model_specification <- function(x, ...) {
  cat("<gp3bayes_model_specification>\n")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Formula: ", x$formula_text, "\n", sep = "")
  cat(
    "  Readiness status: ",
    x$readiness_status,
    "\n",
    sep = ""
  )
  cat(
    "  Readiness warnings: ",
    x$warning_count,
    "\n",
    sep = ""
  )
  cat(
    "  Prior classes: ",
    paste(
      x$priors$table$parameter_class,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  cat("  Backend: none\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

.validate_specification_contract <- function(contract) {
  if (!inherits(contract, "gp3bayes_model_contract")) {
    stop(
      paste(
        "`contract` must inherit from",
        "`gp3bayes_model_contract`."
      ),
      call. = FALSE
    )
  }

  required_fields <- c(
    "family",
    "model_family",
    "mappings",
    "predictors",
    "interaction",
    "random_slope",
    "outcome_unit",
    "prior_rationale"
  )

  missing_fields <- setdiff(
    required_fields,
    names(contract)
  )

  if (length(missing_fields) > 0L) {
    stop(
      paste0(
        "`contract` is missing required fields: ",
        paste(missing_fields, collapse = ", "),
        "."
      ),
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

  if (!is.list(contract$mappings)) {
    stop(
      "`contract$mappings` must be a list.",
      call. = FALSE
    )
  }

  missing_mappings <- setdiff(
    required_mappings,
    names(contract$mappings)
  )

  if (length(missing_mappings) > 0L) {
    stop(
      paste0(
        "`contract$mappings` is missing: ",
        paste(missing_mappings, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  .match_contract_family(
    contract$family
  )

  if (
    !is.character(contract$prior_rationale) ||
    length(contract$prior_rationale) < 4L ||
    anyNA(contract$prior_rationale) ||
    any(!nzchar(contract$prior_rationale))
  ) {
    stop(
      paste(
        "`contract$prior_rationale` must contain at least",
        "four non-empty character values."
      ),
      call. = FALSE
    )
  }

  if (
    !is.logical(contract$random_slope) ||
    length(contract$random_slope) != 1L ||
    is.na(contract$random_slope)
  ) {
    stop(
      "`contract$random_slope` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    contract$random_slope &&
    is.null(contract$mappings$condition)
  ) {
    stop(
      paste(
        "A participant-level random slope requires",
        "`contract$mappings$condition`."
      ),
      call. = FALSE
    )
  }

  invisible(contract)
}

.validate_specification_audit <- function(audit) {
  if (!inherits(audit, "gp3bayes_readiness_audit")) {
    stop(
      paste(
        "`audit` must inherit from",
        "`gp3bayes_readiness_audit`."
      ),
      call. = FALSE
    )
  }

  required_fields <- c(
    "ready",
    "status",
    "status_counts",
    "contract"
  )

  missing_fields <- setdiff(
    required_fields,
    names(audit)
  )

  if (length(missing_fields) > 0L) {
    stop(
      paste0(
        "`audit` is missing required fields: ",
        paste(missing_fields, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.logical(audit$ready) ||
    length(audit$ready) != 1L ||
    is.na(audit$ready)
  ) {
    stop(
      "`audit$ready` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  allowed_statuses <- c(
    "ready",
    "ready_with_warnings",
    "not_ready"
  )

  if (
    !is.character(audit$status) ||
    length(audit$status) != 1L ||
    is.na(audit$status) ||
    !audit$status %in% allowed_statuses
  ) {
    stop(
      "`audit$status` is invalid.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(audit$status_counts) ||
    !all(
      c("pass", "warn", "fail") %in%
        names(audit$status_counts)
    )
  ) {
    stop(
      "`audit$status_counts` is invalid.",
      call. = FALSE
    )
  }

  invisible(audit)
}

.validate_numeric_scalar <- function(
  value,
  argument
) {
  if (
    !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value)
  ) {
    stop(
      paste0(
        "`",
        argument,
        "` must be one finite numeric value."
      ),
      call. = FALSE
    )
  }

  as.numeric(value)
}

.validate_positive_scalar <- function(
  value,
  argument
) {
  value <- .validate_numeric_scalar(
    value,
    argument
  )

  if (value <= 0) {
    stop(
      paste0(
        "`",
        argument,
        "` must be strictly positive."
      ),
      call. = FALSE
    )
  }

  value
}

.validate_probability_scalar <- function(
  value,
  argument
) {
  value <- .validate_numeric_scalar(
    value,
    argument
  )

  if (value <= 0 || value >= 1) {
    stop(
      paste0(
        "`",
        argument,
        "` must be strictly between zero and one."
      ),
      call. = FALSE
    )
  }

  value
}

.quote_formula_name <- function(value) {
  if (
    !is.character(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(value)
  ) {
    stop(
      "Formula column names must be non-empty character scalars.",
      call. = FALSE
    )
  }

  paste(
    deparse(
      as.name(value),
      width.cutoff = 500L,
      backtick = TRUE
    ),
    collapse = ""
  )
}

.formula_to_text <- function(formula) {
  paste(
    deparse(
      formula,
      width.cutoff = 500L
    ),
    collapse = " "
  )
}

.prior_row <- function(
  parameter_class,
  distribution,
  target,
  location = NA_real_,
  scale = NA_real_,
  df = NA_real_,
  shape = NA_real_,
  lower = NA_real_,
  upper = NA_real_,
  rationale
) {
  data.frame(
    parameter_class = parameter_class,
    distribution = distribution,
    target = target,
    location = as.numeric(location),
    scale = as.numeric(scale),
    df = as.numeric(df),
    shape = as.numeric(shape),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    rationale = rationale,
    stringsAsFactors = FALSE
  )
}

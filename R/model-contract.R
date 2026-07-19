#' Create an Approved Bayesian Model Contract
#'
#' Creates an inspectable model-contract object for one of the two model
#' families approved for the initial `gp3bayes` development scope. The
#' function records neutral data-column mappings while preserving the
#' approved likelihood, link, estimands, assumptions, diagnostics,
#' sensitivity requirements, and interpretation boundaries.
#'
#' @param family Character scalar identifying the approved model family.
#'   Supported values are `"binary"` and `"duration"`.
#' @param outcome_col Character scalar naming the outcome column.
#' @param participant_col Character scalar naming the participant identifier
#'   column.
#' @param item_col Optional character scalar naming an item or stimulus
#'   identifier column.
#' @param trial_col Optional character scalar naming a trial identifier
#'   column.
#' @param condition_col Optional character scalar naming the focal condition
#'   column.
#' @param time_col Optional character scalar naming a linear time or trial
#'   order column. This does not define a time-course or autocorrelation
#'   model.
#' @param predictors Character vector naming additional predictors.
#' @param interaction Optional character vector of length two naming one
#'   prespecified two-way interaction. Higher-order or multiple interactions
#'   are not supported by the initial contract.
#' @param random_slope Logical scalar indicating whether one participant-level
#'   random slope for the focal condition is requested. Readiness must be
#'   assessed separately before fitting.
#' @param outcome_unit Optional character scalar recording the outcome unit.
#'   It is required for the duration family and must be `NULL` for the binary
#'   family.
#' @param notes Optional character vector containing user-supplied design or
#'   analysis notes. Notes do not override the approved model contract.
#'
#' @return An object of class `gp3bayes_model_contract`. It is a named list
#'   containing the approved methodological specification, neutral column
#'   mappings, requested model structure, assumptions, diagnostics,
#'   sensitivity requirements, limitations, and unsupported uses.
#'
#' @details
#' The returned object is a specification and audit record. It does not
#' validate a data frame, construct a backend formula, fit a model, or imply
#' that the proposed analysis is appropriate. Those gates are handled by
#' separate workflows.
#'
#' The binary contract uses a Bernoulli likelihood with a logit link. The
#' duration contract uses a lognormal likelihood for strictly positive,
#' finite, uncensored durations.
#'
#' @section Interpretation boundaries:
#' Contract creation does not establish causal identification, model
#' adequacy, convergence, predictive validity, or substantive validity.
#' Behavioural measurements must not be interpreted as direct measures of
#' latent psychological or protected attributes.
#'
#' @examples
#' binary_contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "stimulus_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition"
#' )
#'
#' binary_contract
#'
#' duration_contract <- create_model_contract(
#'   family = "duration",
#'   outcome_col = "response_time",
#'   participant_col = "participant_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition",
#'   outcome_unit = "milliseconds"
#' )
#'
#' duration_contract
#'
#' @export
create_model_contract <- function(
  family,
  outcome_col,
  participant_col,
  item_col = NULL,
  trial_col = NULL,
  condition_col = NULL,
  time_col = NULL,
  predictors = character(),
  interaction = NULL,
  random_slope = FALSE,
  outcome_unit = NULL,
  notes = character()
) {
  family <- .match_contract_family(family)

  mappings <- list(
    outcome = .validate_contract_column(outcome_col, "outcome_col"),
    participant = .validate_contract_column(
      participant_col,
      "participant_col"
    ),
    item = .validate_contract_column(item_col, "item_col", optional = TRUE),
    trial = .validate_contract_column(
      trial_col,
      "trial_col",
      optional = TRUE
    ),
    condition = .validate_contract_column(
      condition_col,
      "condition_col",
      optional = TRUE
    ),
    time = .validate_contract_column(time_col, "time_col", optional = TRUE)
  )

  predictors <- .validate_contract_character_vector(
    predictors,
    "predictors"
  )

  interaction <- .validate_contract_interaction(interaction)
  random_slope <- .validate_contract_flag(random_slope, "random_slope")
  notes <- .validate_contract_character_vector(notes, "notes")

  if (random_slope && is.null(mappings$condition)) {
    stop(
      "`condition_col` must be supplied when `random_slope = TRUE`.",
      call. = FALSE
    )
  }

  if (!is.null(interaction)) {
    available_predictors <- unique(c(
      mappings$condition,
      mappings$time,
      predictors
    ))

    if (!all(interaction %in% available_predictors)) {
      stop(
        paste0(
          "Every interaction variable must be declared through ",
          "`condition_col`, `time_col`, or `predictors`."
        ),
        call. = FALSE
      )
    }
  }

  mapped_columns <- unlist(mappings, use.names = FALSE)
  declared_columns <- c(mapped_columns, predictors)

  if (anyDuplicated(declared_columns)) {
    duplicates <- unique(
      declared_columns[duplicated(declared_columns)]
    )

    stop(
      paste0(
        "Column mappings and predictors must be unique. Duplicated: ",
        paste(duplicates, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  outcome_unit <- .validate_contract_outcome_unit(
    outcome_unit,
    family
  )

  template <- .model_contract_template(family)

  structure(
    c(
      list(
        contract_version = "0.1",
        family = family,
        model_family = .gp3bayes_model_families[[family]],
        mappings = mappings,
        predictors = predictors,
        interaction = interaction,
        random_slope = random_slope,
        outcome_unit = outcome_unit,
        notes = notes
      ),
      template
    ),
    class = "gp3bayes_model_contract"
  )
}

#' Print a gp3bayes Model Contract
#'
#' Prints a concise summary of an approved model contract. The full object
#' remains available for programmatic inspection.
#'
#' @param x A `gp3bayes_model_contract` object.
#' @param ... Additional arguments. They are currently ignored.
#'
#' @return `x`, invisibly.
#'
#' @keywords internal
#' @export
print.gp3bayes_model_contract <- function(x, ...) {
  cat("<gp3bayes_model_contract>\n")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Likelihood: ", x$likelihood, "\n", sep = "")
  cat("  Link: ", x$link, "\n", sep = "")
  cat("  Outcome: ", x$mappings$outcome, "\n", sep = "")
  cat("  Participant: ", x$mappings$participant, "\n", sep = "")

  if (!is.null(x$mappings$item)) {
    cat("  Item: ", x$mappings$item, "\n", sep = "")
  }

  if (!is.null(x$mappings$condition)) {
    cat("  Condition: ", x$mappings$condition, "\n", sep = "")
  }

  if (!is.null(x$outcome_unit)) {
    cat("  Outcome unit: ", x$outcome_unit, "\n", sep = "")
  }

  cat("  Random slope requested: ", x$random_slope, "\n", sep = "")
  cat("  Fitting performed: FALSE\n")

  invisible(x)
}

.match_contract_family <- function(family) {
  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    stop(
      "`family` must be one non-missing character value.",
      call. = FALSE
    )
  }

  if (!family %in% names(.gp3bayes_model_families)) {
    stop(
      paste0(
        "Unsupported `family`: ",
        family,
        ". Supported values are: ",
        paste(names(.gp3bayes_model_families), collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  family
}

.validate_contract_column <- function(
  value,
  argument,
  optional = FALSE
) {
  if (optional && is.null(value)) {
    return(NULL)
  }

  if (
    !is.character(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(value)
  ) {
    stop(
      paste0("`", argument, "` must be one non-empty character value."),
      call. = FALSE
    )
  }

  value
}

.validate_contract_character_vector <- function(value, argument) {
  if (
    !is.character(value) ||
    anyNA(value) ||
    any(!nzchar(value)) ||
    anyDuplicated(value)
  ) {
    stop(
      paste0(
        "`",
        argument,
        "` must be a character vector of unique, non-empty values."
      ),
      call. = FALSE
    )
  }

  value
}

.validate_contract_interaction <- function(interaction) {
  if (is.null(interaction)) {
    return(NULL)
  }

  interaction <- .validate_contract_character_vector(
    interaction,
    "interaction"
  )

  if (length(interaction) != 2L) {
    stop(
      "`interaction` must contain exactly two declared variables.",
      call. = FALSE
    )
  }

  interaction
}

.validate_contract_flag <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(
      paste0("`", argument, "` must be TRUE or FALSE."),
      call. = FALSE
    )
  }

  value
}

.validate_contract_outcome_unit <- function(outcome_unit, family) {
  if (identical(family, "binary")) {
    if (!is.null(outcome_unit)) {
      stop(
        "`outcome_unit` must be NULL for the binary family.",
        call. = FALSE
      )
    }

    return(NULL)
  }

  .validate_contract_column(
    outcome_unit,
    "outcome_unit"
  )
}

.model_contract_template <- function(family) {
  if (identical(family, "binary")) {
    return(.binary_contract_template())
  }

  .duration_contract_template()
}

.binary_contract_template <- function() {
  list(
    intended_outcome = "Binary trial-level event",
    outcome_scale = "0 or 1",
    unit_of_analysis = "One participant-trial or participant-item row",
    grouping_structure = c(
      "Participant random intercept required",
      "Optional crossed item random intercept",
      "At most one participant random slope for the focal condition"
    ),
    repeated_measures_structure = c(
      "Repeated trial-level observations within participant",
      "Optional crossed item or stimulus observations",
      "No automatic serial-correlation model"
    ),
    supported_predictors = c(
      "Focal condition",
      "Optional linear time or trial-order term",
      "Prespecified participant-, item-, or trial-level predictors"
    ),
    supported_interactions = paste(
      "At most one prespecified two-way interaction among",
      "declared predictors"
    ),
    supported_offsets_or_exposures = "Not supported",
    supported_censoring = "Not applicable",
    likelihood = "Bernoulli",
    link = "logit",
    estimands = c(
      "Population-level conditional log-odds contrast",
      "Conditional odds ratio",
      "Design-standardised predicted-probability contrast"
    ),
    coefficient_interpretation = paste(
      "Population-level conditional log-odds contrast per declared",
      "predictor unit"
    ),
    prior_families = c(
      "Normal prior for the intercept on the logit scale",
      "Normal priors for population-level coefficients",
      "Half-Student-t priors for group-level standard deviations",
      "LKJ prior for a supported random-effect correlation"
    ),
    prior_rationale = c(
      paste(
        "The intercept prior must encode plausible baseline event",
        "probabilities on the logit scale"
      ),
      paste(
        "Coefficient priors regularise implausibly large log-odds",
        "contrasts while retaining substantive uncertainty"
      ),
      paste(
        "Group-level scale priors constrain extreme heterogeneity",
        "without fixing it to zero"
      ),
      paste(
        "The correlation prior regularises an included",
        "intercept-slope correlation"
      )
    ),
    scaling_expectations = c(
      "No silent scaling",
      "Record all centring and scaling decisions",
      "Identifiers must not be used as numeric predictors"
    ),
    assumptions = c(
      "Conditional Bernoulli sampling is appropriate",
      "The approved grouping structure represents repeated observations",
      "The design supports the requested contrast",
      "Important serial dependence is not left unmodelled"
    ),
    convergence_criteria = c(
      "R-hat no greater than 1.01",
      "Adequate bulk and tail effective sample sizes",
      "Zero divergent transitions",
      "Zero maximum-treedepth saturations",
      "Acceptable energy and chain-mixing diagnostics"
    ),
    prior_predictive_checks = c(
      "Overall and condition-specific event rates",
      "Participant-level and item-level rate dispersion",
      "Implausibly extreme probability or contrast frequency"
    ),
    posterior_predictive_checks = c(
      "Overall and condition-specific event rates",
      "Participant-level and item-level rate distributions",
      "Sparse-cell and all-zero or all-one pattern frequencies"
    ),
    sensitivity_requirements = c(
      "Population-level coefficient prior scales",
      "Group-level standard-deviation priors",
      "Random-intercept versus approved random-slope specification",
      "Influential participant and item cases"
    ),
    limitations = c(
      "No automatic model selection",
      "No causal interpretation without an identifying design",
      "No transition, time-course, or arbitrary nonlinear structure"
    ),
    unsupported_uses = c(
      "Multinomial or ordinal outcomes",
      "Aggregated proportions without denominators",
      "Survey weights or automatic variable selection",
      "Psychological or protected-attribute inference"
    ),
    interpretation_boundaries = c(
      paste(
        "Contract creation does not establish model adequacy",
        "or convergence"
      ),
      paste(
        "Associations are not causal effects without an",
        "identifying design"
      ),
      paste(
        "Behavioural measurements do not directly identify latent",
        "psychological or protected attributes"
      )
    ),
    computational_requirements = paste(
      "Contract creation is backend-independent; fitting requires an",
      "approved optional backend"
    )
  )
}

.duration_contract_template <- function() {
  list(
    intended_outcome = "Strictly positive uncensored duration",
    outcome_scale = "Finite continuous value greater than zero",
    unit_of_analysis = "One participant-trial, participant-item, or event row",
    grouping_structure = c(
      "Participant random intercept required",
      "Optional crossed item random intercept",
      "At most one participant random slope for the focal condition"
    ),
    repeated_measures_structure = c(
      "Repeated duration observations within participant",
      "Optional crossed item or stimulus observations",
      "No automatic serial-correlation model"
    ),
    supported_predictors = c(
      "Focal condition",
      "Optional linear time or trial-order term",
      "Prespecified participant-, item-, or trial-level predictors"
    ),
    supported_interactions = paste(
      "At most one prespecified two-way interaction among",
      "declared predictors"
    ),
    supported_offsets_or_exposures = "Not supported",
    supported_censoring = paste(
      "Not supported; durations must be strictly positive",
      "and uncensored"
    ),
    likelihood = "lognormal",
    link = "identity on mean log duration",
    estimands = c(
      "Population-level contrast in expected log duration",
      "Conditional median ratio",
      "Design-standardised predictive median difference or ratio",
      "Prespecified posterior predictive upper quantile"
    ),
    coefficient_interpretation = paste(
      "Population-level contrast on the log-duration scale; exponentiation",
      "gives a conditional median ratio"
    ),
    prior_families = c(
      "Normal prior for log baseline median",
      "Normal priors for population-level coefficients",
      "Half-Student-t priors for residual and group-level scales",
      "LKJ prior for a supported random-effect correlation"
    ),
    prior_rationale = c(
      paste(
        "The intercept prior must reflect a plausible baseline median",
        "duration in the recorded unit after log transformation"
      ),
      paste(
        "Coefficient priors regularise implausible multiplicative",
        "duration contrasts"
      ),
      paste(
        "Residual and group-level scale priors constrain implausible",
        "dispersion without fixing variability to zero"
      ),
      paste(
        "The correlation prior regularises an included",
        "intercept-slope correlation"
      )
    ),
    scaling_expectations = c(
      "Record the duration unit",
      "No silent unit conversion",
      "Record all centring and scaling decisions",
      "Identifiers must not be used as numeric predictors"
    ),
    assumptions = c(
      "Durations are strictly positive, finite, and uncensored",
      "Conditional log durations are adequately Gaussian",
      "The approved grouping structure represents repeated observations",
      "No important mixture, deadline, or serial process is omitted"
    ),
    convergence_criteria = c(
      "R-hat no greater than 1.01",
      "Adequate bulk and tail effective sample sizes",
      "Zero divergent transitions",
      "Zero maximum-treedepth saturations",
      "Acceptable energy and chain-mixing diagnostics"
    ),
    prior_predictive_checks = c(
      "Median and interquartile range",
      "Upper quantiles and tail exceedance",
      "Implausibly small or large duration frequency",
      "Participant-level and item-level dispersion"
    ),
    posterior_predictive_checks = c(
      "Raw-scale and log-scale distribution",
      "Condition-specific medians and upper quantiles",
      "Participant-level and item-level distributions",
      "Within-participant condition contrasts"
    ),
    sensitivity_requirements = c(
      "Population-level coefficient prior scales",
      "Residual and group-level scale priors",
      "Random-intercept versus approved random-slope specification",
      "Influential participant and item cases",
      "Duration-unit rescaling invariance"
    ),
    limitations = c(
      "No automatic distribution switching",
      "No causal interpretation without an identifying design",
      "No time-course, autocorrelation, or distributional regression"
    ),
    unsupported_uses = c(
      "Zero, censored, or truncated durations",
      "Shifted-lognormal, Gamma, Weibull, survival, or mixture models",
      "Automatic variable selection",
      "Psychological or protected-attribute inference"
    ),
    interpretation_boundaries = c(
      paste(
        "Contract creation does not establish model adequacy",
        "or convergence"
      ),
      paste(
        "Exponentiated coefficients are conditional median ratios",
        "and not automatically raw-mean ratios"
      ),
      paste(
        "Associations are not causal effects without an",
        "identifying design"
      ),
      paste(
        "Behavioural measurements do not directly identify latent",
        "psychological or protected attributes"
      )
    ),
    computational_requirements = paste(
      "Contract creation is backend-independent; fitting requires an",
      "approved optional backend"
    )
  )
}


.gp3b_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3b_assert_numeric_scalar <- function(
  value,
  name,
  lower = -Inf,
  upper = Inf,
  lower_open = FALSE,
  upper_open = FALSE
) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    .gp3b_stop("`", name, "` must be one finite numeric value.")
  }

  lower_ok <- if (lower_open) value > lower else value >= lower
  upper_ok <- if (upper_open) value < upper else value <= upper

  if (!lower_ok || !upper_ok) {
    interval <- paste0(
      if (lower_open) "(" else "[",
      lower,
      ", ",
      upper,
      if (upper_open) ")" else "]"
    )

    .gp3b_stop("`", name, "` must lie in ", interval, ".")
  }

  as.numeric(value)
}

.gp3b_assert_integer <- function(value, name, minimum = 1L) {
  value <- .gp3b_assert_numeric_scalar(
    value,
    name,
    lower = minimum
  )

  if (value != floor(value)) {
    .gp3b_stop("`", name, "` must be integer-valued.")
  }

  as.integer(value)
}

.gp3b_assert_flag <- function(value, name) {
  if (
    !is.logical(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    .gp3b_stop("`", name, "` must be TRUE or FALSE.")
  }

  value
}

.gp3b_assert_character_vector <- function(value, name) {
  if (is.null(value)) {
    return(character())
  }

  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      anyDuplicated(value)
  ) {
    .gp3b_stop(
      "`",
      name,
      "` must be a character vector of unique non-empty values."
    )
  }

  value
}

.gp3b_validate_contract <- function(contract, binary = TRUE) {
  if (!inherits(contract, "gp3bayes_model_contract")) {
    .gp3b_stop(
      "`contract` must inherit from `gp3bayes_model_contract`."
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

  missing_fields <- setdiff(required_fields, names(contract))

  if (length(missing_fields) > 0L) {
    .gp3b_stop(
      "`contract` is missing required fields: ",
      paste(missing_fields, collapse = ", "),
      "."
    )
  }

  if (binary && !identical(contract$family, "binary")) {
    .gp3b_stop("This workflow requires a binary model contract.")
  }

  invisible(contract)
}

.gp3b_quote_name <- function(value) {
  paste(
    deparse(
      as.name(value),
      width.cutoff = 500L,
      backtick = TRUE
    ),
    collapse = ""
  )
}

.gp3b_formula_text <- function(formula) {
  paste(
    deparse(
      formula,
      width.cutoff = 500L
    ),
    collapse = " "
  )
}

.gp3b_fixed_formula <- function(contract) {
  .gp3b_validate_contract(contract)

  fixed_terms <- unique(
    c(
      contract$mappings$condition,
      contract$mappings$time,
      contract$predictors
    )
  )

  fixed_terms <- fixed_terms[
    !is.na(fixed_terms) & nzchar(fixed_terms)
  ]

  quoted_terms <- vapply(
    fixed_terms,
    .gp3b_quote_name,
    character(1)
  )

  interaction_term <- character()

  if (!is.null(contract$interaction)) {
    interaction_term <- paste(
      vapply(
        contract$interaction,
        .gp3b_quote_name,
        character(1)
      ),
      collapse = ":"
    )
  }

  right_hand_side <- unique(
    c(
      quoted_terms,
      interaction_term
    )
  )

  if (length(right_hand_side) == 0L) {
    right_hand_side <- "1"
  }

  formula_text <- paste0(
    .gp3b_quote_name(contract$mappings$outcome),
    " ~ ",
    paste(right_hand_side, collapse = " + ")
  )

  stats::as.formula(
    formula_text,
    env = baseenv()
  )
}

.gp3b_map_binary_outcome <- function(x, mapping) {
  observed <- x[!is.na(x)]

  if (is.logical(x)) {
    return(as.integer(x))
  }

  if (
    is.numeric(x) &&
      length(observed) > 0L &&
      all(observed %in% c(0, 1))
  ) {
    return(as.integer(x))
  }

  if (is.null(mapping)) {
    .gp3b_stop(
      "A named `outcome_mapping` with values 0 and 1 is required ",
      "for labelled binary outcomes."
    )
  }

  if (
    !is.atomic(mapping) ||
      length(mapping) != 2L ||
      is.null(names(mapping)) ||
      anyNA(mapping) ||
      anyNA(names(mapping)) ||
      any(!nzchar(names(mapping))) ||
      anyDuplicated(names(mapping)) ||
      !setequal(as.numeric(mapping), c(0, 1))
  ) {
    .gp3b_stop(
      "`outcome_mapping` must be a named vector mapping exactly ",
      "two distinct labels to 0 and 1."
    )
  }

  values <- as.character(x)
  observed_labels <- unique(values[!is.na(values)])

  if (!all(observed_labels %in% names(mapping))) {
    missing_labels <- setdiff(observed_labels, names(mapping))

    .gp3b_stop(
      "`outcome_mapping` does not cover observed labels: ",
      paste(missing_labels, collapse = ", "),
      "."
    )
  }

  as.integer(unname(mapping[values]))
}

.gp3b_code_condition <- function(
  x,
  condition_levels = NULL,
  condition_coding = c(-0.5, 0.5)
) {
  condition_coding <- as.numeric(condition_coding)

  if (
    length(condition_coding) != 2L ||
      anyNA(condition_coding) ||
      any(!is.finite(condition_coding)) ||
      condition_coding[[1L]] == condition_coding[[2L]]
  ) {
    .gp3b_stop(
      "`condition_coding` must contain two distinct finite numeric values."
    )
  }

  observed <- unique(x[!is.na(x)])

  if (length(observed) != 2L) {
    .gp3b_stop(
      "The focal condition must have exactly two observed non-missing levels."
    )
  }

  if (is.null(condition_levels)) {
    if (is.factor(x)) {
      declared_levels <- levels(x)
      condition_levels <- declared_levels[
        declared_levels %in% as.character(observed)
      ]
    } else if (is.numeric(x)) {
      condition_levels <- sort(observed)
    } else {
      condition_levels <- sort(as.character(observed))
    }
  }

  if (
    length(condition_levels) != 2L ||
      anyNA(condition_levels) ||
      !setequal(as.character(observed), as.character(condition_levels))
  ) {
    .gp3b_stop(
      "`condition_levels` must list the two observed levels in ",
      "reference-to-focal order."
    )
  }

  mapped <- condition_coding[
    match(
      as.character(x),
      as.character(condition_levels)
    )
  ]

  list(
    values = as.numeric(mapped),
    source_levels = as.character(condition_levels),
    coding = stats::setNames(
      condition_coding,
      as.character(condition_levels)
    )
  )
}

.gp3b_prior_row <- function(priors, parameter_class) {
  validate_prior_specification(
    priors,
    contract = NULL
  )

  rows <- priors$table$parameter_class == parameter_class

  if (sum(rows) != 1L) {
    .gp3b_stop(
      "The prior specification must contain exactly one `",
      parameter_class,
      "` row."
    )
  }

  priors$table[rows, , drop = FALSE]
}

.gp3b_half_student_t <- function(n, df, scale) {
  abs(stats::rt(n, df = df)) * scale
}

.gp3b_binary_summary <- function(
  y,
  probability,
  condition,
  participant,
  item = NULL,
  boundary_probability = c(0.01, 0.99)
) {
  participant_rates <- tapply(y, participant, mean)

  condition_low_rate <- NA_real_
  condition_high_rate <- NA_real_
  condition_rate_contrast <- NA_real_

  if (!is.null(condition)) {
    condition_levels <- sort(unique(condition))

    if (length(condition_levels) == 2L) {
      condition_rates <- vapply(
        condition_levels,
        function(level) {
          mean(y[condition == level])
        },
        numeric(1)
      )

      condition_low_rate <- condition_rates[[1L]]
      condition_high_rate <- condition_rates[[2L]]
      condition_rate_contrast <-
        condition_high_rate - condition_low_rate
    }
  }

  item_rate_sd <- NA_real_

  if (!is.null(item)) {
    item_rates <- tapply(y, item, mean)

    if (length(item_rates) > 1L) {
      item_rate_sd <- stats::sd(item_rates)
    }
  }

  c(
    overall_rate = mean(y),
    condition_low_rate = condition_low_rate,
    condition_high_rate = condition_high_rate,
    condition_rate_contrast = condition_rate_contrast,
    participant_rate_sd = if (length(participant_rates) > 1L) {
      stats::sd(participant_rates)
    } else {
      NA_real_
    },
    item_rate_sd = item_rate_sd,
    participant_all_zero = mean(participant_rates == 0),
    participant_all_one = mean(participant_rates == 1),
    probability_below_boundary = mean(
      probability < boundary_probability[[1L]]
    ),
    probability_above_boundary = mean(
      probability > boundary_probability[[2L]]
    )
  )
}

#' Simulate Hierarchical Binary Data
#'
#' Generates deterministic synthetic repeated-measures data from the approved
#' Bernoulli-logit contract. The simulator records all generating parameters,
#' participant effects, optional crossed item effects, and the random-number
#' seed. It performs no model fitting.
#'
#' @param n_participants Number of participants.
#' @param trials_per_participant Number of observations per participant.
#' @param n_items Number of crossed items when `include_items = TRUE`.
#' @param intercept Population intercept on the log-odds scale.
#' @param condition_effect Population condition contrast on the log-odds scale.
#' @param participant_covariate_effect Participant-covariate coefficient.
#' @param trial_covariate_effect Trial-covariate coefficient.
#' @param interaction_effect Condition-by-participant-covariate coefficient.
#' @param participant_sd Participant random-intercept standard deviation.
#' @param item_sd Crossed item random-intercept standard deviation.
#' @param random_slope_sd Participant condition-slope standard deviation.
#' @param random_slope_cor Correlation between participant intercepts and
#'   condition slopes. It must lie strictly between -1 and 1.
#' @param condition_probability Treatment probability when
#'   `balanced_condition = FALSE`.
#' @param balanced_condition Whether each participant receives an approximately
#'   balanced condition sequence.
#' @param include_items Whether to generate a crossed item identifier.
#' @param seed Non-negative integer random-number seed.
#'
#' @return A `gp3bayes_binary_simulation` containing synthetic data, stored
#'   truth, generated random effects, and design metadata.
#'
#' @details
#' The condition is generated using `-0.5` and `0.5` internally and returned
#' as a factor with levels `control` and `treatment`. The data-generating model
#' includes one participant random intercept, one optional correlated
#' participant condition slope, and one optional crossed item intercept.
#'
#' @examples
#' simulation <- simulate_hierarchical_binary_data(
#'   n_participants = 12,
#'   trials_per_participant = 8,
#'   n_items = 6,
#'   seed = 2026
#' )
#'
#' simulation
#' head(simulation$data)
#'
#' @export
simulate_hierarchical_binary_data <- function(
  n_participants = 40,
  trials_per_participant = 20,
  n_items = 20,
  intercept = stats::qlogis(0.35),
  condition_effect = 0.8,
  participant_covariate_effect = 0.3,
  trial_covariate_effect = 0.15,
  interaction_effect = 0.25,
  participant_sd = 0.7,
  item_sd = 0.35,
  random_slope_sd = 0.3,
  random_slope_cor = 0,
  condition_probability = 0.5,
  balanced_condition = TRUE,
  include_items = TRUE,
  seed = 1
) {
  n_participants <- .gp3b_assert_integer(
    n_participants,
    "n_participants",
    minimum = 2L
  )
  trials_per_participant <- .gp3b_assert_integer(
    trials_per_participant,
    "trials_per_participant",
    minimum = 2L
  )
  n_items <- .gp3b_assert_integer(
    n_items,
    "n_items",
    minimum = 2L
  )
  intercept <- .gp3b_assert_numeric_scalar(intercept, "intercept")
  condition_effect <- .gp3b_assert_numeric_scalar(
    condition_effect,
    "condition_effect"
  )
  participant_covariate_effect <- .gp3b_assert_numeric_scalar(
    participant_covariate_effect,
    "participant_covariate_effect"
  )
  trial_covariate_effect <- .gp3b_assert_numeric_scalar(
    trial_covariate_effect,
    "trial_covariate_effect"
  )
  interaction_effect <- .gp3b_assert_numeric_scalar(
    interaction_effect,
    "interaction_effect"
  )
  participant_sd <- .gp3b_assert_numeric_scalar(
    participant_sd,
    "participant_sd",
    lower = 0
  )
  item_sd <- .gp3b_assert_numeric_scalar(
    item_sd,
    "item_sd",
    lower = 0
  )
  random_slope_sd <- .gp3b_assert_numeric_scalar(
    random_slope_sd,
    "random_slope_sd",
    lower = 0
  )
  random_slope_cor <- .gp3b_assert_numeric_scalar(
    random_slope_cor,
    "random_slope_cor",
    lower = -1,
    upper = 1,
    lower_open = TRUE,
    upper_open = TRUE
  )
  condition_probability <- .gp3b_assert_numeric_scalar(
    condition_probability,
    "condition_probability",
    lower = 0,
    upper = 1,
    lower_open = TRUE,
    upper_open = TRUE
  )
  balanced_condition <- .gp3b_assert_flag(
    balanced_condition,
    "balanced_condition"
  )
  include_items <- .gp3b_assert_flag(
    include_items,
    "include_items"
  )
  seed <- .gp3b_assert_integer(seed, "seed", minimum = 0L)

  set.seed(seed)

  participant_levels <- sprintf(
    "p%03d",
    seq_len(n_participants)
  )
  participant_id <- rep(
    participant_levels,
    each = trials_per_participant
  )
  trial_id <- rep(
    seq_len(trials_per_participant),
    times = n_participants
  )
  n_rows <- length(participant_id)

  if (balanced_condition) {
    condition_code <- unlist(
      lapply(
        seq_len(n_participants),
        function(index) {
          values <- rep(
            c(-0.5, 0.5),
            length.out = trials_per_participant
          )

          sample(
            values,
            size = length(values),
            replace = FALSE
          )
        }
      ),
      use.names = FALSE
    )
  } else {
    condition_code <- ifelse(
      stats::runif(n_rows) < condition_probability,
      0.5,
      -0.5
    )
  }

  participant_covariate_by_id <- stats::rnorm(n_participants)
  participant_covariate_by_id <- as.numeric(
    scale(participant_covariate_by_id)
  )
  participant_index <- match(
    participant_id,
    participant_levels
  )
  participant_covariate <- participant_covariate_by_id[
    participant_index
  ]

  trial_covariate <- as.numeric(
    scale(stats::rnorm(n_rows))
  )

  z_intercept <- stats::rnorm(n_participants)
  z_slope <- stats::rnorm(n_participants)

  participant_intercept <- participant_sd * z_intercept
  participant_slope <- random_slope_sd * (
    random_slope_cor * z_intercept +
      sqrt(1 - random_slope_cor^2) * z_slope
  )

  item_id <- NULL
  item_levels <- character()
  item_effect_by_id <- numeric()
  item_effect <- rep(0, n_rows)

  if (include_items) {
    item_levels <- sprintf(
      "i%03d",
      seq_len(n_items)
    )

    item_index <- (
      trial_id +
        rep(seq_len(n_participants) - 1L, each = trials_per_participant) -
        1L
    ) %% n_items + 1L

    item_id <- item_levels[item_index]
    item_effect_by_id <- stats::rnorm(
      n_items,
      mean = 0,
      sd = item_sd
    )
    item_effect <- item_effect_by_id[item_index]
  }

  linear_predictor <- intercept +
    condition_effect * condition_code +
    participant_covariate_effect * participant_covariate +
    trial_covariate_effect * trial_covariate +
    interaction_effect * condition_code * participant_covariate +
    participant_intercept[participant_index] +
    participant_slope[participant_index] * condition_code +
    item_effect

  probability <- stats::plogis(linear_predictor)
  selected <- stats::rbinom(
    n_rows,
    size = 1L,
    prob = probability
  )

  data <- data.frame(
    participant_id = participant_id,
    trial_id = trial_id,
    condition = factor(
      ifelse(
        condition_code < 0,
        "control",
        "treatment"
      ),
      levels = c(
        "control",
        "treatment"
      )
    ),
    participant_covariate = participant_covariate,
    trial_covariate = trial_covariate,
    selected = selected,
    true_probability = probability,
    stringsAsFactors = FALSE
  )

  if (include_items) {
    data$item_id <- item_id
  }

  preferred_order <- c(
    "participant_id",
    if (include_items) "item_id",
    "trial_id",
    "condition",
    "participant_covariate",
    "trial_covariate",
    "selected",
    "true_probability"
  )

  data <- data[preferred_order]

  truth <- list(
    fixed_effects = c(
      `(Intercept)` = intercept,
      condition = condition_effect,
      participant_covariate = participant_covariate_effect,
      trial_covariate = trial_covariate_effect,
      `condition:participant_covariate` = interaction_effect
    ),
    participant_sd = participant_sd,
    item_sd = if (include_items) item_sd else 0,
    random_slope_sd = random_slope_sd,
    random_slope_cor = random_slope_cor,
    baseline_probability = stats::plogis(intercept),
    condition_coding = c(
      control = -0.5,
      treatment = 0.5
    ),
    condition_probability = condition_probability,
    balanced_condition = balanced_condition,
    seed = seed
  )

  random_effects <- list(
    participant = data.frame(
      participant_id = participant_levels,
      intercept = participant_intercept,
      condition_slope = participant_slope,
      participant_covariate = participant_covariate_by_id,
      stringsAsFactors = FALSE
    ),
    item = if (include_items) {
      data.frame(
        item_id = item_levels,
        intercept = item_effect_by_id,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  )

  structure(
    list(
      simulation_version = "0.1",
      data = data,
      truth = truth,
      random_effects = random_effects,
      design = list(
        n_participants = n_participants,
        trials_per_participant = trials_per_participant,
        n_items = if (include_items) n_items else 0L,
        include_items = include_items,
        random_slope = random_slope_sd > 0,
        row_count = n_rows
      )
    ),
    class = "gp3bayes_binary_simulation"
  )
}

#' Prepare Hierarchical Binary Data
#'
#' Applies explicit binary outcome mapping, explicit two-level condition
#' coding, optional recorded numeric scaling, and a model-readiness gate.
#' No variable is silently scaled or recoded.
#'
#' @param data A data frame containing the columns declared in `contract`.
#' @param contract A binary `gp3bayes_model_contract`.
#' @param outcome_mapping Optional named vector mapping two labelled outcome
#'   values to 0 and 1. It is required for non-logical, non-0/1 outcomes.
#' @param condition_levels Optional two-value vector listing the condition
#'   levels in reference-to-focal order.
#' @param condition_coding Two distinct finite numeric values used to encode
#'   the declared condition. The default is `c(-0.5, 0.5)`.
#' @param scale_predictors Character vector naming declared numeric predictors
#'   to centre and divide by their sample standard deviation.
#' @param scale_time Whether to centre and scale the declared linear time
#'   variable.
#' @param missing Either `"error"` or `"drop"`. Dropping is performed only
#'   after this explicit argument is selected, and removed row positions are
#'   recorded.
#'
#' @return A `gp3bayes_binary_prepared` object containing the analysis data,
#'   contract, readiness audit, transformation registry, fixed-effects formula,
#'   design-matrix columns, and row accounting.
#'
#' @details
#' This function performs deterministic preparation only. It does not fit a
#' model, create posterior draws, or establish causal or substantive validity.
#'
#' @examples
#' simulation <- simulate_hierarchical_binary_data(
#'   n_participants = 12,
#'   trials_per_participant = 8,
#'   seed = 2026
#' )
#'
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "item_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition",
#'   predictors = c(
#'     "participant_covariate",
#'     "trial_covariate"
#'   ),
#'   interaction = c(
#'     "condition",
#'     "participant_covariate"
#'   ),
#'   random_slope = TRUE
#' )
#'
#' prepared <- prepare_hierarchical_binary_data(
#'   simulation$data,
#'   contract,
#'   condition_levels = c("control", "treatment")
#' )
#'
#' prepared
#'
#' @export
prepare_hierarchical_binary_data <- function(
  data,
  contract,
  outcome_mapping = NULL,
  condition_levels = NULL,
  condition_coding = c(-0.5, 0.5),
  scale_predictors = character(),
  scale_time = FALSE,
  missing = c("error", "drop")
) {
  if (!is.data.frame(data)) {
    .gp3b_stop("`data` must be a data frame.")
  }

  .gp3b_validate_contract(contract)

  missing <- match.arg(missing)
  scale_predictors <- .gp3b_assert_character_vector(
    scale_predictors,
    "scale_predictors"
  )
  scale_time <- .gp3b_assert_flag(scale_time, "scale_time")

  if (!all(scale_predictors %in% contract$predictors)) {
    undeclared <- setdiff(
      scale_predictors,
      contract$predictors
    )

    .gp3b_stop(
      "Every `scale_predictors` entry must be declared in ",
      "`contract$predictors`. Undeclared: ",
      paste(undeclared, collapse = ", "),
      "."
    )
  }

  required_columns <- unique(
    c(
      unlist(contract$mappings, use.names = FALSE),
      contract$predictors,
      contract$interaction
    )
  )

  required_columns <- required_columns[
    !is.na(required_columns) & nzchar(required_columns)
  ]

  absent_columns <- setdiff(required_columns, names(data))

  if (length(absent_columns) > 0L) {
    .gp3b_stop(
      "Required columns are missing: ",
      paste(absent_columns, collapse = ", "),
      "."
    )
  }

  working <- data
  complete_rows <- stats::complete.cases(
    working[required_columns]
  )
  dropped_rows <- which(!complete_rows)

  if (length(dropped_rows) > 0L) {
    if (identical(missing, "error")) {
      .gp3b_stop(
        "Missing values are present in declared analysis columns. ",
        "Use `missing = \"drop\"` only after an explicit decision."
      )
    }

    working <- working[
      complete_rows,
      ,
      drop = FALSE
    ]
  }

  if (nrow(working) == 0L) {
    .gp3b_stop("No complete analysis rows remain.")
  }

  outcome_column <- contract$mappings$outcome
  original_outcome_type <- class(working[[outcome_column]])
  working[[outcome_column]] <- .gp3b_map_binary_outcome(
    working[[outcome_column]],
    outcome_mapping
  )

  transformations <- list(
    outcome = list(
      column = outcome_column,
      original_class = original_outcome_type,
      mapping = if (is.null(outcome_mapping)) {
        c(`0` = 0L, `1` = 1L)
      } else {
        outcome_mapping
      }
    ),
    condition = NULL,
    numeric_scaling = list(),
    missing = list(
      action = missing,
      dropped_row_positions = dropped_rows,
      retained_row_positions = which(complete_rows)
    )
  )

  condition_column <- contract$mappings$condition

  if (!is.null(condition_column)) {
    condition_result <- .gp3b_code_condition(
      working[[condition_column]],
      condition_levels = condition_levels,
      condition_coding = condition_coding
    )

    working[[condition_column]] <- condition_result$values
    transformations$condition <- list(
      column = condition_column,
      source_levels = condition_result$source_levels,
      coding = condition_result$coding
    )
  }

  columns_to_scale <- scale_predictors

  if (
    scale_time &&
      !is.null(contract$mappings$time)
  ) {
    columns_to_scale <- unique(
      c(
        columns_to_scale,
        contract$mappings$time
      )
    )
  }

  for (column in columns_to_scale) {
    values <- working[[column]]

    if (
      !is.numeric(values) ||
        any(!is.finite(values))
    ) {
      .gp3b_stop(
        "Scaled column `",
        column,
        "` must be finite and numeric."
      )
    }

    center <- mean(values)
    scale_value <- stats::sd(values)

    if (
      !is.finite(scale_value) ||
        scale_value <= 0
    ) {
      .gp3b_stop(
        "Scaled column `",
        column,
        "` must have positive variation."
      )
    }

    working[[column]] <- (
      values - center
    ) / scale_value

    transformations$numeric_scaling[[column]] <- c(
      center = center,
      scale = scale_value
    )
  }

  audit <- audit_model_readiness(
    working,
    contract
  )

  if (!isTRUE(audit$ready)) {
    .gp3b_stop(
      "Prepared data failed the model-readiness gate with status `",
      audit$status,
      "`."
    )
  }

  fixed_formula <- .gp3b_fixed_formula(contract)
  model_matrix <- stats::model.matrix(
    fixed_formula,
    data = working
  )

  if (qr(model_matrix)$rank < ncol(model_matrix)) {
    .gp3b_stop(
      "The prepared fixed-effects design matrix is rank deficient."
    )
  }

  decision_log <- data.frame(
    decision = c(
      "binary_outcome_mapping",
      "condition_coding",
      "numeric_scaling",
      "missing_rows"
    ),
    value = c(
      paste(
        names(transformations$outcome$mapping),
        transformations$outcome$mapping,
        sep = "=",
        collapse = "; "
      ),
      if (is.null(transformations$condition)) {
        "not_applicable"
      } else {
        paste(
          names(transformations$condition$coding),
          transformations$condition$coding,
          sep = "=",
          collapse = "; "
        )
      },
      if (length(transformations$numeric_scaling) == 0L) {
        "none"
      } else {
        paste(
          names(transformations$numeric_scaling),
          collapse = ", "
        )
      },
      paste0(
        missing,
        "; dropped=",
        length(dropped_rows)
      )
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      preparation_version = "0.1",
      data = working,
      contract = contract,
      audit = audit,
      transformations = transformations,
      decision_log = decision_log,
      fixed_formula = fixed_formula,
      fixed_formula_text = .gp3b_formula_text(fixed_formula),
      model_matrix_columns = colnames(model_matrix),
      n_input_rows = nrow(data),
      n_analysis_rows = nrow(working),
      rows_removed = length(dropped_rows),
      contains_data = TRUE,
      backend = "none",
      fit_performed = FALSE
    ),
    class = "gp3bayes_binary_prepared"
  )
}

#' Specify a Backend-Independent Binary Model
#'
#' Combines prepared binary data, a successful readiness audit, the restricted
#' hierarchical formula, and validated family-specific priors. The returned
#' object is not executable and performs no model fitting.
#'
#' @param prepared A `gp3bayes_binary_prepared` object.
#' @param baseline Plausible baseline event probability.
#' @param intercept_scale Optional scale for the normal intercept prior.
#' @param coefficient_scale Optional common scale for normal population-level
#'   coefficient priors, including the approved interaction.
#' @param group_sd_scale Scale for half-Student-t group standard deviations.
#' @param correlation_eta LKJ shape used when a random slope is requested.
#' @param student_df Degrees of freedom for half-Student-t scale priors.
#'
#' @return A `gp3bayes_binary_model_specification` that also inherits from
#'   `gp3bayes_model_specification`.
#'
#' @details
#' The specification retains the prepared data because backend-independent
#' prior predictive simulation must reproduce the declared design. It contains
#' no backend object, posterior draws, or fitted model.
#'
#' @examples
#' simulation <- simulate_hierarchical_binary_data(
#'   n_participants = 12,
#'   trials_per_participant = 8,
#'   seed = 2026
#' )
#'
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "item_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition"
#' )
#'
#' prepared <- prepare_hierarchical_binary_data(
#'   simulation$data,
#'   contract,
#'   condition_levels = c("control", "treatment")
#' )
#'
#' specification <- specify_binary_model(
#'   prepared,
#'   baseline = 0.35
#' )
#'
#' specification
#'
#' @export
specify_binary_model <- function(
  prepared,
  baseline = 0.5,
  intercept_scale = 1.5,
  coefficient_scale = 0.75,
  group_sd_scale = 1,
  correlation_eta = 2,
  student_df = 3
) {
  if (!inherits(prepared, "gp3bayes_binary_prepared")) {
    .gp3b_stop(
      "`prepared` must inherit from `gp3bayes_binary_prepared`."
    )
  }

  .gp3b_validate_contract(prepared$contract)

  if (!isTRUE(prepared$audit$ready)) {
    .gp3b_stop("`prepared$audit` is not ready for specification.")
  }

  priors <- create_prior_specification(
    prepared$contract,
    baseline = baseline,
    intercept_scale = intercept_scale,
    coefficient_scale = coefficient_scale,
    group_sd_scale = group_sd_scale,
    correlation_eta = correlation_eta,
    student_df = student_df
  )

  core <- create_model_specification(
    prepared$contract,
    prepared$audit,
    priors
  )

  core$binary_workflow_version <- "0.1"
  core$prepared <- prepared
  core$fixed_formula <- prepared$fixed_formula
  core$fixed_formula_text <- prepared$fixed_formula_text
  core$model_matrix_columns <- prepared$model_matrix_columns
  core$contains_data <- TRUE
  core$fitting_engine <- "none"
  core$backend_dependency <- "none"
  core$unrestricted_formula <- FALSE
  core$prior_predictive_performed <- FALSE

  class(core) <- c(
    "gp3bayes_binary_model_specification",
    class(core)
  )

  core
}

#' Check Binary Prior Predictive Behaviour
#'
#' Simulates replicated binary outcomes from the declared prior specification
#' and prepared design without calling a Bayesian fitting backend.
#'
#' @param specification A `gp3bayes_binary_model_specification`.
#' @param draws Number of prior predictive data sets.
#' @param seed Non-negative integer random-number seed.
#' @param plausible_rate Increasing lower and upper limits for plausible
#'   overall and condition-specific event rates.
#' @param boundary_probability Probability thresholds used to identify prior
#'   mass close to zero and one.
#' @param extreme_contrast Absolute probability-scale condition contrast
#'   considered extreme.
#' @param maximum_degenerate_participant_fraction Maximum participant fraction
#'   allowed to have all-zero or all-one replicated outcomes in a draw.
#' @param maximum_boundary_mass Maximum fraction of row probabilities allowed
#'   beyond the declared boundary thresholds in a draw.
#' @param maximum_extreme_probability Maximum acceptable fraction of prior
#'   predictive draws violating each criterion.
#'
#' @return A `gp3bayes_binary_prior_predictive_check` containing replicated
#'   summaries, structured checks, thresholds, and the seed.
#'
#' @details
#' Failure does not select or alter priors automatically. It indicates that the
#' declared priors and design generate outcomes that require substantive review.
#' This check assesses prior implications, not posterior adequacy or model fit.
#'
#' @examples
#' simulation <- simulate_hierarchical_binary_data(
#'   n_participants = 12,
#'   trials_per_participant = 8,
#'   seed = 2026
#' )
#'
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "item_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition"
#' )
#'
#' prepared <- prepare_hierarchical_binary_data(
#'   simulation$data,
#'   contract,
#'   condition_levels = c("control", "treatment")
#' )
#'
#' specification <- specify_binary_model(
#'   prepared,
#'   baseline = 0.35
#' )
#'
#' check_binary_prior_predictive(
#'   specification,
#'   draws = 100,
#'   seed = 2027
#' )
#'
#' @export
check_binary_prior_predictive <- function(
  specification,
  draws = 500,
  seed = 1,
  plausible_rate = c(0.01, 0.99),
  boundary_probability = c(0.01, 0.99),
  extreme_contrast = 0.8,
  maximum_degenerate_participant_fraction = 0.5,
  maximum_boundary_mass = 0.5,
  maximum_extreme_probability = 0.25
) {
  if (
    !inherits(
      specification,
      "gp3bayes_binary_model_specification"
    )
  ) {
    .gp3b_stop(
      "`specification` must inherit from ",
      "`gp3bayes_binary_model_specification`."
    )
  }

  prepared <- specification$prepared

  if (!inherits(prepared, "gp3bayes_binary_prepared")) {
    .gp3b_stop(
      "`specification$prepared` must inherit from ",
      "`gp3bayes_binary_prepared`."
    )
  }

  validate_prior_specification(
    specification$priors,
    contract = specification$contract
  )

  draws <- .gp3b_assert_integer(draws, "draws", minimum = 50L)
  seed <- .gp3b_assert_integer(seed, "seed", minimum = 0L)

  plausible_rate <- as.numeric(plausible_rate)
  boundary_probability <- as.numeric(boundary_probability)

  valid_probability_pair <- function(values, name) {
    if (
      length(values) != 2L ||
        anyNA(values) ||
        any(!is.finite(values)) ||
        values[[1L]] < 0 ||
        values[[2L]] > 1 ||
        values[[1L]] >= values[[2L]]
    ) {
      .gp3b_stop(
        "`",
        name,
        "` must be an increasing pair between zero and one."
      )
    }

    values
  }

  plausible_rate <- valid_probability_pair(
    plausible_rate,
    "plausible_rate"
  )
  boundary_probability <- valid_probability_pair(
    boundary_probability,
    "boundary_probability"
  )
  extreme_contrast <- .gp3b_assert_numeric_scalar(
    extreme_contrast,
    "extreme_contrast",
    lower = 0,
    upper = 1
  )
  maximum_degenerate_participant_fraction <-
    .gp3b_assert_numeric_scalar(
      maximum_degenerate_participant_fraction,
      "maximum_degenerate_participant_fraction",
      lower = 0,
      upper = 1
    )
  maximum_boundary_mass <- .gp3b_assert_numeric_scalar(
    maximum_boundary_mass,
    "maximum_boundary_mass",
    lower = 0,
    upper = 1
  )
  maximum_extreme_probability <- .gp3b_assert_numeric_scalar(
    maximum_extreme_probability,
    "maximum_extreme_probability",
    lower = 0,
    upper = 1
  )

  data <- prepared$data
  contract <- specification$contract
  fixed_formula <- prepared$fixed_formula
  model_matrix <- stats::model.matrix(
    fixed_formula,
    data = data
  )

  participant_column <- contract$mappings$participant
  participant <- factor(data[[participant_column]])
  participant_index <- as.integer(participant)
  participant_count <- nlevels(participant)

  item <- NULL
  item_index <- NULL
  item_count <- 0L

  if (!is.null(contract$mappings$item)) {
    item <- factor(data[[contract$mappings$item]])
    item_index <- as.integer(item)
    item_count <- nlevels(item)
  }

  condition <- NULL

  if (!is.null(contract$mappings$condition)) {
    condition <- as.numeric(
      data[[contract$mappings$condition]]
    )
  }

  intercept_prior <- .gp3b_prior_row(
    specification$priors,
    "Intercept"
  )
  coefficient_prior <- .gp3b_prior_row(
    specification$priors,
    "b"
  )
  group_sd_prior <- .gp3b_prior_row(
    specification$priors,
    "sd"
  )

  correlation_prior <- NULL

  if (isTRUE(contract$random_slope)) {
    correlation_prior <- .gp3b_prior_row(
      specification$priors,
      "cor"
    )
  }

  set.seed(seed)

  summary_names <- names(
    .gp3b_binary_summary(
      y = rep(0L, nrow(data)),
      probability = rep(0.5, nrow(data)),
      condition = condition,
      participant = participant,
      item = item,
      boundary_probability = boundary_probability
    )
  )

  summaries <- matrix(
    NA_real_,
    nrow = draws,
    ncol = length(summary_names),
    dimnames = list(
      NULL,
      summary_names
    )
  )

  for (draw_index in seq_len(draws)) {
    coefficients <- stats::rnorm(
      ncol(model_matrix),
      mean = coefficient_prior$location[[1L]],
      sd = coefficient_prior$scale[[1L]]
    )

    intercept_index <- which(
      colnames(model_matrix) == "(Intercept)"
    )

    if (length(intercept_index) == 1L) {
      coefficients[[intercept_index]] <- stats::rnorm(
        1L,
        mean = intercept_prior$location[[1L]],
        sd = intercept_prior$scale[[1L]]
      )
    }

    participant_intercept_sd <- .gp3b_half_student_t(
      1L,
      df = group_sd_prior$df[[1L]],
      scale = group_sd_prior$scale[[1L]]
    )
    z_intercept <- stats::rnorm(participant_count)
    participant_intercept <-
      participant_intercept_sd * z_intercept

    linear_predictor <- as.vector(
      model_matrix %*% coefficients
    ) + participant_intercept[participant_index]

    if (isTRUE(contract$random_slope)) {
      participant_slope_sd <- .gp3b_half_student_t(
        1L,
        df = group_sd_prior$df[[1L]],
        scale = group_sd_prior$scale[[1L]]
      )
      correlation <- 2 * stats::rbeta(
        1L,
        correlation_prior$shape[[1L]],
        correlation_prior$shape[[1L]]
      ) - 1
      z_slope <- stats::rnorm(participant_count)
      participant_slope <- participant_slope_sd * (
        correlation * z_intercept +
          sqrt(1 - correlation^2) * z_slope
      )

      linear_predictor <- linear_predictor +
        participant_slope[participant_index] * condition
    }

    if (!is.null(item)) {
      item_sd <- .gp3b_half_student_t(
        1L,
        df = group_sd_prior$df[[1L]],
        scale = group_sd_prior$scale[[1L]]
      )
      item_intercept <- stats::rnorm(
        item_count,
        mean = 0,
        sd = item_sd
      )
      linear_predictor <- linear_predictor +
        item_intercept[item_index]
    }

    probability <- stats::plogis(linear_predictor)
    replicated_outcome <- stats::rbinom(
      nrow(data),
      size = 1L,
      prob = probability
    )

    summaries[draw_index, ] <- .gp3b_binary_summary(
      y = replicated_outcome,
      probability = probability,
      condition = condition,
      participant = participant,
      item = item,
      boundary_probability = boundary_probability
    )
  }

  summaries <- as.data.frame(
    summaries,
    stringsAsFactors = FALSE
  )

  overall_rate_probability <- mean(
    summaries$overall_rate < plausible_rate[[1L]] |
      summaries$overall_rate > plausible_rate[[2L]]
  )

  condition_available <- !is.null(condition)

  condition_rate_probability <- if (condition_available) {
    mean(
      summaries$condition_low_rate < plausible_rate[[1L]] |
        summaries$condition_low_rate > plausible_rate[[2L]] |
        summaries$condition_high_rate < plausible_rate[[1L]] |
        summaries$condition_high_rate > plausible_rate[[2L]],
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  condition_contrast_probability <- if (condition_available) {
    mean(
      abs(summaries$condition_rate_contrast) > extreme_contrast,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  participant_degeneracy_probability <- mean(
    summaries$participant_all_zero +
      summaries$participant_all_one >
      maximum_degenerate_participant_fraction
  )

  boundary_mass_probability <- mean(
    summaries$probability_below_boundary +
      summaries$probability_above_boundary >
      maximum_boundary_mass
  )

  check_names <- c(
    "overall_rate",
    "condition_rates",
    "condition_contrast",
    "participant_degeneracy",
    "boundary_probability_mass"
  )
  probabilities <- c(
    overall_rate_probability,
    condition_rate_probability,
    condition_contrast_probability,
    participant_degeneracy_probability,
    boundary_mass_probability
  )
  applicable <- c(
    TRUE,
    condition_available,
    condition_available,
    TRUE,
    TRUE
  )
  pass <- applicable &
    is.finite(probabilities) &
    probabilities <= maximum_extreme_probability

  checks <- data.frame(
    check = check_names,
    probability = probabilities,
    threshold = rep(
      maximum_extreme_probability,
      length(check_names)
    ),
    status = ifelse(
      !applicable,
      "not_applicable",
      ifelse(pass, "pass", "fail")
    ),
    stringsAsFactors = FALSE
  )

  adequate <- all(
    checks$status[checks$status != "not_applicable"] == "pass"
  )

  structure(
    list(
      check_version = "0.1",
      family = "binary",
      adequate = adequate,
      draws = draws,
      summaries = summaries,
      checks = checks,
      thresholds = list(
        plausible_rate = plausible_rate,
        boundary_probability = boundary_probability,
        extreme_contrast = extreme_contrast,
        maximum_degenerate_participant_fraction =
          maximum_degenerate_participant_fraction,
        maximum_boundary_mass = maximum_boundary_mass,
        maximum_extreme_probability = maximum_extreme_probability
      ),
      seed = seed,
      backend = "none",
      fitting_performed = FALSE,
      interpretation = paste(
        "Failure indicates that the declared priors generate outcomes",
        "requiring substantive review under the approved design; priors",
        "are not altered or selected automatically."
      ),
      limitations = paste(
        "This is a prior predictive simulation, not evidence of posterior",
        "adequacy, convergence, causal identification, or substantive validity."
      )
    ),
    class = c(
      "gp3bayes_binary_prior_predictive_check",
      "gp3bayes_prior_predictive_check"
    )
  )
}

#' @export
print.gp3bayes_binary_simulation <- function(x, ...) {
  cat("<gp3bayes_binary_simulation>\n")
  cat("  Rows: ", nrow(x$data), "\n", sep = "")
  cat(
    "  Participants: ",
    x$design$n_participants,
    "\n",
    sep = ""
  )
  cat("  Items: ", x$design$n_items, "\n", sep = "")
  cat(
    "  True condition effect: ",
    x$truth$fixed_effects[["condition"]],
    "\n",
    sep = ""
  )
  cat("  Seed: ", x$truth$seed, "\n", sep = "")

  invisible(x)
}

#' @export
print.gp3bayes_binary_prepared <- function(x, ...) {
  cat("<gp3bayes_binary_prepared>\n")
  cat("  Input rows: ", x$n_input_rows, "\n", sep = "")
  cat("  Analysis rows: ", x$n_analysis_rows, "\n", sep = "")
  cat("  Rows removed: ", x$rows_removed, "\n", sep = "")
  cat("  Readiness: ", x$audit$status, "\n", sep = "")
  cat(
    "  Fixed matrix columns: ",
    paste(x$model_matrix_columns, collapse = ", "),
    "\n",
    sep = ""
  )
  cat("  Backend: none\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_binary_model_specification <- function(x, ...) {
  cat("<gp3bayes_binary_model_specification>\n")
  cat("  Formula: ", x$formula_text, "\n", sep = "")
  cat(
    "  Fixed formula: ",
    x$fixed_formula_text,
    "\n",
    sep = ""
  )
  cat(
    "  Baseline probability: ",
    x$priors$baseline,
    "\n",
    sep = ""
  )
  cat("  Readiness: ", x$readiness_status, "\n", sep = "")
  cat("  Fitting engine: none\n")
  cat("  Backend dependency: none\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_binary_prior_predictive_check <- function(x, ...) {
  cat("<gp3bayes_binary_prior_predictive_check>\n")
  cat("  Adequate: ", x$adequate, "\n", sep = "")
  cat("  Draws: ", x$draws, "\n", sep = "")
  cat(
    "  Failed checks: ",
    sum(x$checks$status == "fail"),
    "\n",
    sep = ""
  )
  cat("  Backend: none\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

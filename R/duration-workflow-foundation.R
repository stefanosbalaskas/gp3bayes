.gp3d_validate_contract <- function(contract) {
  if (!inherits(
    contract,
    "gp3bayes_model_contract"
  )) {
    .gp3b_stop(
      "`contract` must inherit from `gp3bayes_model_contract`."
    )
  }

  if (!identical(
    contract$family,
    "duration"
  )) {
    .gp3b_stop(
      "This workflow requires a duration model contract."
    )
  }

  if (!identical(
    contract$likelihood,
    "lognormal"
  )) {
    .gp3b_stop(
      "The duration workflow requires the approved lognormal likelihood."
    )
  }

  if (
    is.null(contract$outcome_unit) ||
      !is.character(contract$outcome_unit) ||
      length(contract$outcome_unit) != 1L ||
      is.na(contract$outcome_unit) ||
      !nzchar(contract$outcome_unit)
  ) {
    .gp3b_stop(
      "A duration contract must record one non-empty `outcome_unit`."
    )
  }

  invisible(contract)
}

.gp3d_fixed_formula <- function(contract) {
  .gp3d_validate_contract(
    contract
  )

  fixed_terms <- unique(
    c(
      contract$mappings$condition,
      contract$mappings$time,
      contract$predictors
    )
  )

  fixed_terms <- fixed_terms[
    !is.na(fixed_terms) &
      nzchar(fixed_terms)
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

  stats::as.formula(
    paste0(
      .gp3b_quote_name(
        contract$mappings$outcome
      ),
      " ~ ",
      paste(
        right_hand_side,
        collapse = " + "
      )
    ),
    env = baseenv()
  )
}

.gp3d_required_columns <- function(contract) {
  mapped <- unlist(
    contract$mappings,
    use.names = FALSE
  )

  mapped <- mapped[
    !is.na(mapped) &
      nzchar(mapped)
  ]

  unique(
    c(
      mapped,
      contract$predictors,
      contract$interaction
    )
  )
}

.gp3d_numeric_outcome <- function(x) {
  if (is.numeric(x)) {
    return(
      as.numeric(x)
    )
  }

  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}

.gp3d_scale_column <- function(
  data,
  column,
  registry
) {
  values <- data[[column]]

  if (!is.numeric(values)) {
    .gp3b_stop(
      "Only numeric predictors can be scaled. `",
      column,
      "` is not numeric."
    )
  }

  centre <- mean(values)
  scale <- stats::sd(values)

  if (
    !is.finite(scale) ||
      scale <= 0
  ) {
    .gp3b_stop(
      "The declared scaling column `",
      column,
      "` has zero or undefined standard deviation."
    )
  }

  data[[column]] <- (
    values - centre
  ) / scale

  registry[[column]] <- list(
    action = "centre_and_scale",
    centre = centre,
    scale = scale
  )

  list(
    data = data,
    registry = registry
  )
}

.gp3d_duration_summary <- function(
  y,
  condition,
  participant,
  item = NULL
) {
  valid <- is.finite(y) & y > 0
  nonfinite_fraction <- mean(!valid)

  if (!any(valid)) {
    return(
      c(
        median = Inf,
        mean = Inf,
        q90 = Inf,
        q99 = Inf,
        coefficient_of_variation = Inf,
        condition_median_ratio = NA_real_,
        participant_log_median_sd = NA_real_,
        item_log_median_sd = NA_real_,
        nonfinite_fraction =
          nonfinite_fraction
      )
    )
  }

  y_valid <- y[valid]
  condition_valid <- if (
    is.null(condition)
  ) {
    NULL
  } else {
    condition[valid]
  }
  participant_valid <-
    participant[valid]
  item_valid <- if (
    is.null(item)
  ) {
    NULL
  } else {
    item[valid]
  }

  log_y <- log(y_valid)

  participant_medians <- tapply(
    log_y,
    participant_valid,
    stats::median
  )

  condition_median_ratio <- NA_real_

  if (!is.null(condition_valid)) {
    levels <- sort(
      unique(condition_valid)
    )

    if (length(levels) == 2L) {
      medians <- vapply(
        levels,
        function(level) {
          stats::median(
            y_valid[
              condition_valid == level
            ]
          )
        },
        numeric(1)
      )

      if (
        all(is.finite(medians)) &&
          medians[[1L]] > 0
      ) {
        condition_median_ratio <-
          medians[[2L]] /
          medians[[1L]]
      }
    }
  }

  item_log_median_sd <- NA_real_

  if (!is.null(item_valid)) {
    item_medians <- tapply(
      log_y,
      item_valid,
      stats::median
    )

    if (length(item_medians) > 1L) {
      item_log_median_sd <-
        stats::sd(item_medians)
    }
  }

  mean_y <- mean(y_valid)

  c(
    median = stats::median(y_valid),
    mean = mean_y,
    q90 = stats::quantile(
      y_valid,
      probs = 0.90,
      names = FALSE,
      type = 8
    ),
    q99 = stats::quantile(
      y_valid,
      probs = 0.99,
      names = FALSE,
      type = 8
    ),
    coefficient_of_variation =
      if (
        length(y_valid) > 1L &&
          is.finite(mean_y) &&
          mean_y > 0
      ) {
        stats::sd(y_valid) /
          mean_y
      } else {
        NA_real_
      },
    condition_median_ratio =
      condition_median_ratio,
    participant_log_median_sd =
      if (
        length(participant_medians) > 1L
      ) {
        stats::sd(
          participant_medians
        )
      } else {
        NA_real_
      },
    item_log_median_sd =
      item_log_median_sd,
    nonfinite_fraction =
      nonfinite_fraction
  )
}

#' Simulate Hierarchical Lognormal Duration Data
#'
#' Generates deterministic strictly positive uncensored durations from the
#' approved hierarchical lognormal contract.
#'
#' @param n_participants Number of participants.
#' @param trials_per_participant Observations per participant.
#' @param n_items Number of crossed items when `include_items = TRUE`.
#' @param baseline_median Baseline duration median in `outcome_unit`.
#' @param condition_effect Population condition contrast on the log-duration
#'   scale.
#' @param participant_covariate_effect Participant-covariate coefficient on the
#'   log-duration scale.
#' @param trial_covariate_effect Trial-covariate coefficient on the
#'   log-duration scale.
#' @param interaction_effect Condition-by-participant-covariate coefficient on
#'   the log-duration scale.
#' @param participant_sd Participant random-intercept standard deviation on the
#'   log scale.
#' @param item_sd Crossed item random-intercept standard deviation.
#' @param random_slope_sd Participant condition-slope standard deviation.
#' @param random_slope_cor Correlation between participant intercepts and
#'   condition slopes.
#' @param residual_sd Lognormal residual standard deviation.
#' @param condition_probability Focal-condition probability for an unbalanced
#'   design.
#' @param balanced_condition Whether each participant receives an approximately
#'   balanced condition sequence.
#' @param include_items Whether crossed items are generated.
#' @param outcome_unit Recorded duration unit.
#' @param seed Non-negative integer random-number seed.
#'
#' @return A `gp3bayes_duration_simulation` containing synthetic data, stored
#'   truth, random effects, and design metadata.
#'
#' @details
#' The generated outcome is strictly positive, finite, and uncensored. The
#' function does not generate zero, censored, truncated, shifted, or survival
#' outcomes.
#'
#' @export
simulate_hierarchical_duration_data <- function(
  n_participants = 40L,
  trials_per_participant = 20L,
  n_items = 20L,
  baseline_median = 500,
  condition_effect = log(1.15),
  participant_covariate_effect =
    log(1.08),
  trial_covariate_effect =
    log(1.04),
  interaction_effect =
    log(1.05),
  participant_sd = 0.35,
  item_sd = 0.20,
  random_slope_sd = 0.15,
  random_slope_cor = 0,
  residual_sd = 0.40,
  condition_probability = 0.5,
  balanced_condition = TRUE,
  include_items = TRUE,
  outcome_unit = "milliseconds",
  seed = 1L
) {
  n_participants <- .gp3b_assert_integer(
    n_participants,
    "n_participants",
    minimum = 2L
  )
  trials_per_participant <-
    .gp3b_assert_integer(
      trials_per_participant,
      "trials_per_participant",
      minimum = 2L
    )
  n_items <- .gp3b_assert_integer(
    n_items,
    "n_items",
    minimum = 2L
  )
  baseline_median <-
    .gp3b_assert_numeric_scalar(
      baseline_median,
      "baseline_median",
      lower = 0,
      lower_open = TRUE
    )
  condition_effect <-
    .gp3b_assert_numeric_scalar(
      condition_effect,
      "condition_effect"
    )
  participant_covariate_effect <-
    .gp3b_assert_numeric_scalar(
      participant_covariate_effect,
      "participant_covariate_effect"
    )
  trial_covariate_effect <-
    .gp3b_assert_numeric_scalar(
      trial_covariate_effect,
      "trial_covariate_effect"
    )
  interaction_effect <-
    .gp3b_assert_numeric_scalar(
      interaction_effect,
      "interaction_effect"
    )
  participant_sd <-
    .gp3b_assert_numeric_scalar(
      participant_sd,
      "participant_sd",
      lower = 0
    )
  item_sd <- .gp3b_assert_numeric_scalar(
    item_sd,
    "item_sd",
    lower = 0
  )
  random_slope_sd <-
    .gp3b_assert_numeric_scalar(
      random_slope_sd,
      "random_slope_sd",
      lower = 0
    )
  random_slope_cor <-
    .gp3b_assert_numeric_scalar(
      random_slope_cor,
      "random_slope_cor",
      lower = -1,
      upper = 1,
      lower_open = TRUE,
      upper_open = TRUE
    )
  residual_sd <-
    .gp3b_assert_numeric_scalar(
      residual_sd,
      "residual_sd",
      lower = 0,
      lower_open = TRUE
    )
  condition_probability <-
    .gp3b_assert_numeric_scalar(
      condition_probability,
      "condition_probability",
      lower = 0,
      upper = 1,
      lower_open = TRUE,
      upper_open = TRUE
    )
  balanced_condition <-
    .gp3b_assert_flag(
      balanced_condition,
      "balanced_condition"
    )
  include_items <- .gp3b_assert_flag(
    include_items,
    "include_items"
  )
  seed <- .gp3b_assert_integer(
    seed,
    "seed",
    minimum = 0L
  )

  if (
    !is.character(outcome_unit) ||
      length(outcome_unit) != 1L ||
      is.na(outcome_unit) ||
      !nzchar(outcome_unit)
  ) {
    .gp3b_stop(
      "`outcome_unit` must be one non-empty character value."
    )
  }

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
  n_rows <- length(
    participant_id
  )

  if (balanced_condition) {
    condition_code <- unlist(
      lapply(
        seq_len(n_participants),
        function(index) {
          sample(
            rep(
              c(
                -0.5,
                0.5
              ),
              length.out =
                trials_per_participant
            ),
            size =
              trials_per_participant,
            replace = FALSE
          )
        }
      ),
      use.names = FALSE
    )
  } else {
    condition_code <- ifelse(
      stats::runif(n_rows) <
        condition_probability,
      0.5,
      -0.5
    )
  }

  participant_covariate_by_id <-
    stats::rnorm(
      n_participants
    )
  participant_covariate_by_id <-
    as.numeric(
      scale(
        participant_covariate_by_id
      )
    )
  participant_index <- match(
    participant_id,
    participant_levels
  )
  participant_covariate <-
    participant_covariate_by_id[
      participant_index
    ]

  trial_covariate <- as.numeric(
    scale(
      stats::rnorm(n_rows)
    )
  )

  z_intercept <- stats::rnorm(
    n_participants
  )
  z_slope <- stats::rnorm(
    n_participants
  )

  participant_intercept <-
    participant_sd * z_intercept
  participant_slope <-
    random_slope_sd * (
      random_slope_cor *
        z_intercept +
        sqrt(
          1 -
            random_slope_cor^2
        ) * z_slope
    )

  item_id <- NULL
  item_levels <- character()
  item_effect_by_id <- numeric()
  item_effect <- rep(
    0,
    n_rows
  )

  if (include_items) {
    item_levels <- sprintf(
      "i%03d",
      seq_len(n_items)
    )
    item_index <- (
      trial_id +
        rep(
          seq_len(n_participants) -
            1L,
          each =
            trials_per_participant
        ) -
        1L
    ) %% n_items + 1L
    item_id <-
      item_levels[item_index]
    item_effect_by_id <-
      stats::rnorm(
        n_items,
        mean = 0,
        sd = item_sd
      )
    item_effect <-
      item_effect_by_id[
        item_index
      ]
  }

  linear_predictor <-
    log(baseline_median) +
    condition_effect *
      condition_code +
    participant_covariate_effect *
      participant_covariate +
    trial_covariate_effect *
      trial_covariate +
    interaction_effect *
      condition_code *
      participant_covariate +
    participant_intercept[
      participant_index
    ] +
    participant_slope[
      participant_index
    ] *
      condition_code +
    item_effect

  duration <- stats::rlnorm(
    n_rows,
    meanlog = linear_predictor,
    sdlog = residual_sd
  )

  data <- data.frame(
    participant_id =
      participant_id,
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
    participant_covariate =
      participant_covariate,
    trial_covariate =
      trial_covariate,
    duration = duration,
    true_median =
      exp(linear_predictor),
    true_mean =
      exp(
        linear_predictor +
          residual_sd^2 / 2
      ),
    stringsAsFactors = FALSE
  )

  if (include_items) {
    data$item_id <- item_id
  }

  preferred_order <- c(
    "participant_id",
    if (include_items) {
      "item_id"
    },
    "trial_id",
    "condition",
    "participant_covariate",
    "trial_covariate",
    "duration",
    "true_median",
    "true_mean"
  )

  data <- data[
    preferred_order
  ]

  truth <- list(
    fixed_effects = c(
      `(Intercept)` =
        log(baseline_median),
      condition =
        condition_effect,
      participant_covariate =
        participant_covariate_effect,
      trial_covariate =
        trial_covariate_effect,
      `condition:participant_covariate` =
        interaction_effect
    ),
    baseline_median =
      baseline_median,
    participant_sd =
      participant_sd,
    item_sd =
      if (include_items) {
        item_sd
      } else {
        0
      },
    random_slope_sd =
      random_slope_sd,
    random_slope_cor =
      random_slope_cor,
    residual_sd =
      residual_sd,
    condition_coding = c(
      control = -0.5,
      treatment = 0.5
    ),
    condition_probability =
      condition_probability,
    balanced_condition =
      balanced_condition,
    outcome_unit =
      outcome_unit,
    seed = seed
  )

  random_effects <- list(
    participant = data.frame(
      participant_id =
        participant_levels,
      intercept =
        participant_intercept,
      condition_slope =
        participant_slope,
      participant_covariate =
        participant_covariate_by_id,
      stringsAsFactors = FALSE
    ),
    item = if (include_items) {
      data.frame(
        item_id = item_levels,
        intercept =
          item_effect_by_id,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  )

  structure(
    list(
      simulation_version = "0.1",
      family = "duration",
      data = data,
      truth = truth,
      random_effects =
        random_effects,
      design = list(
        n_participants =
          n_participants,
        trials_per_participant =
          trials_per_participant,
        n_items =
          if (include_items) {
            n_items
          } else {
            0L
          },
        include_items =
          include_items,
        random_slope =
          random_slope_sd > 0,
        row_count = n_rows,
        outcome_unit =
          outcome_unit,
        censored = FALSE
      )
    ),
    class =
      "gp3bayes_duration_simulation"
  )
}

#' Prepare Hierarchical Duration Data
#'
#' Validates strictly positive finite uncensored durations, applies explicit
#' unit conversion and recorded scaling, and runs the duration readiness gate.
#'
#' @param data A data frame containing columns declared in `contract`.
#' @param contract A duration `gp3bayes_model_contract`.
#' @param condition_levels Optional two-value condition order.
#' @param condition_coding Two distinct finite numeric condition codes.
#' @param scale_predictors Declared numeric predictors to centre and scale.
#' @param scale_time Whether to centre and scale the declared linear time
#'   variable.
#' @param outcome_multiplier Positive unit-conversion multiplier.
#' @param converted_unit Required destination unit when
#'   `outcome_multiplier != 1`.
#' @param missing Either `"error"` or `"drop"`.
#'
#' @return A `gp3bayes_duration_prepared` object.
#'
#' @details
#' Zero, negative, non-finite, censored, truncated, or shifted durations are not
#' supported. Unit conversion is never inferred.
#'
#' @export
prepare_hierarchical_duration_data <- function(
  data,
  contract,
  condition_levels = NULL,
  condition_coding = c(
    -0.5,
    0.5
  ),
  scale_predictors = character(),
  scale_time = FALSE,
  outcome_multiplier = 1,
  converted_unit = NULL,
  missing = c(
    "error",
    "drop"
  )
) {
  if (!is.data.frame(data)) {
    .gp3b_stop(
      "`data` must be a data frame."
    )
  }

  .gp3d_validate_contract(
    contract
  )

  missing <- match.arg(
    missing
  )
  scale_predictors <-
    .gp3b_assert_character_vector(
      scale_predictors,
      "scale_predictors"
    )
  scale_time <- .gp3b_assert_flag(
    scale_time,
    "scale_time"
  )
  outcome_multiplier <-
    .gp3b_assert_numeric_scalar(
      outcome_multiplier,
      "outcome_multiplier",
      lower = 0,
      lower_open = TRUE
    )

  if (
    !all(
      scale_predictors %in%
        contract$predictors
    )
  ) {
    .gp3b_stop(
      "Every scaled predictor must be declared in the model contract."
    )
  }

  if (
    !isTRUE(
      all.equal(
        outcome_multiplier,
        1
      )
    )
  ) {
    if (
      !is.character(converted_unit) ||
        length(converted_unit) != 1L ||
        is.na(converted_unit) ||
        !nzchar(converted_unit)
    ) {
      .gp3b_stop(
        "`converted_unit` must be supplied when ",
        "`outcome_multiplier` is not one."
      )
    }
  } else if (!is.null(converted_unit)) {
    if (
      !is.character(converted_unit) ||
        length(converted_unit) != 1L ||
        is.na(converted_unit) ||
        !nzchar(converted_unit)
    ) {
      .gp3b_stop(
        "`converted_unit` must be NULL or one non-empty character value."
      )
    }
  }

  required <- .gp3d_required_columns(
    contract
  )
  absent <- setdiff(
    required,
    names(data)
  )

  if (length(absent) > 0L) {
    .gp3b_stop(
      "Required duration columns were not found: ",
      paste(
        absent,
        collapse = ", "
      ),
      "."
    )
  }

  working <- data
  missing_rows <- which(
    !stats::complete.cases(
      working[
        required
      ]
    )
  )

  if (
    length(missing_rows) > 0L &&
      identical(
        missing,
        "error"
      )
  ) {
    .gp3b_stop(
      "Missing values were found in required duration columns. ",
      "Use `missing = \"drop\"` only after an explicit exclusion decision."
    )
  }

  if (
    length(missing_rows) > 0L &&
      identical(
        missing,
        "drop"
      )
  ) {
    working <- working[
      -missing_rows,
      ,
      drop = FALSE
    ]
  }

  outcome <-
    contract$mappings$outcome
  converted <-
    .gp3d_numeric_outcome(
      working[[outcome]]
    )

  if (
    anyNA(converted) ||
      any(!is.finite(converted))
  ) {
    .gp3b_stop(
      "The duration outcome must contain only finite numeric values."
    )
  }

  converted <-
    converted *
      outcome_multiplier

  if (
    any(converted <= 0)
  ) {
    .gp3b_stop(
      "The duration outcome must be strictly positive. ",
      "Zero and negative values are unsupported."
    )
  }

  working[[outcome]] <- converted

  analysis_contract <- contract

  if (!is.null(converted_unit)) {
    analysis_contract$outcome_unit <-
      converted_unit
  }

  transformations <- list(
    outcome = list(
      source_unit =
        contract$outcome_unit,
      analysis_unit =
        analysis_contract$outcome_unit,
      multiplier =
        outcome_multiplier,
      strictly_positive = TRUE,
      finite = TRUE,
      censored = FALSE
    ),
    condition = NULL,
    scaled_columns = list(),
    missing = list(
      action = missing,
      dropped_row_positions =
        as.integer(missing_rows)
    )
  )

  if (
    !is.null(
      analysis_contract$mappings$condition
    )
  ) {
    condition_column <-
      analysis_contract$mappings$condition
    coded <- .gp3b_code_condition(
      working[[condition_column]],
      condition_levels =
        condition_levels,
      condition_coding =
        condition_coding
    )
    working[[condition_column]] <- coded$values
    transformations$condition <-
      coded[
        c(
          "source_levels",
          "coding"
        )
      ]
  }

  scaling_columns <- unique(
    c(
      scale_predictors,
      if (
        scale_time &&
          !is.null(
            analysis_contract$mappings$time
          )
      ) {
        analysis_contract$mappings$time
      }
    )
  )

  for (
    column in scaling_columns
  ) {
    scaled <- .gp3d_scale_column(
      working,
      column,
      transformations$scaled_columns
    )
    working <- scaled$data
    transformations$scaled_columns <-
      scaled$registry
  }

  audit <- audit_model_readiness(
    working,
    analysis_contract
  )

  if (!isTRUE(audit$ready)) {
    .gp3b_stop(
      "The prepared duration data did not pass the readiness gate."
    )
  }

  fixed_formula <-
    .gp3d_fixed_formula(
      analysis_contract
    )
  model_matrix <- stats::model.matrix(
    fixed_formula,
    data = working
  )

  decision_log <- data.frame(
    decision = c(
      "outcome_validation",
      "unit_conversion",
      "missing_values",
      "condition_coding",
      "predictor_scaling"
    ),
    value = c(
      "strictly positive finite uncensored",
      paste0(
        contract$outcome_unit,
        " x ",
        outcome_multiplier,
        " -> ",
        analysis_contract$outcome_unit
      ),
      paste0(
        missing,
        "; rows removed = ",
        length(missing_rows)
      ),
      if (
        is.null(
          transformations$condition
        )
      ) {
        "not applicable"
      } else {
        paste(
          names(
            transformations$condition$coding
          ),
          transformations$condition$coding,
          sep = "=",
          collapse = ", "
        )
      },
      if (
        length(scaling_columns) == 0L
      ) {
        "none"
      } else {
        paste(
          scaling_columns,
          collapse = ", "
        )
      }
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      preparation_version = "0.1",
      family = "duration",
      data = working,
      source_contract = contract,
      contract =
        analysis_contract,
      audit = audit,
      transformations =
        transformations,
      decision_log =
        decision_log,
      fixed_formula =
        fixed_formula,
      fixed_formula_text =
        .gp3b_formula_text(
          fixed_formula
        ),
      model_matrix_columns =
        colnames(model_matrix),
      n_input_rows =
        nrow(data),
      n_analysis_rows =
        nrow(working),
      rows_removed =
        length(missing_rows),
      outcome_unit =
        analysis_contract$outcome_unit,
      contains_data = TRUE,
      backend = "none",
      fit_performed = FALSE
    ),
    class =
      "gp3bayes_duration_prepared"
  )
}

#' Specify a Backend-Independent Duration Model
#'
#' Combines prepared positive-duration data with the approved hierarchical
#' lognormal formula and explicit prior specification.
#'
#' @param prepared A `gp3bayes_duration_prepared`.
#' @param baseline Plausible baseline median in the prepared outcome unit.
#' @param intercept_scale Positive normal-intercept prior scale.
#' @param coefficient_scale Positive population-coefficient prior scale.
#' @param group_sd_scale Positive group standard-deviation prior scale.
#' @param residual_scale Positive lognormal residual-scale prior scale.
#' @param correlation_eta LKJ shape for an approved random slope.
#' @param student_df Degrees of freedom for half-Student-t scale priors.
#'
#' @return A `gp3bayes_duration_model_specification`.
#'
#' @export
specify_duration_model <- function(
  prepared,
  baseline,
  intercept_scale = 1,
  coefficient_scale = 0.5,
  group_sd_scale = 1,
  residual_scale = 1,
  correlation_eta = 2,
  student_df = 3
) {
  if (!inherits(
    prepared,
    "gp3bayes_duration_prepared"
  )) {
    .gp3b_stop(
      "`prepared` must inherit from `gp3bayes_duration_prepared`."
    )
  }

  .gp3d_validate_contract(
    prepared$contract
  )

  if (!isTRUE(
    prepared$audit$ready
  )) {
    .gp3b_stop(
      "`prepared$audit` is not ready for specification."
    )
  }

  priors <- create_prior_specification(
    prepared$contract,
    baseline = baseline,
    intercept_scale =
      intercept_scale,
    coefficient_scale =
      coefficient_scale,
    group_sd_scale =
      group_sd_scale,
    residual_scale =
      residual_scale,
    correlation_eta =
      correlation_eta,
    student_df = student_df
  )

  core <- create_model_specification(
    prepared$contract,
    prepared$audit,
    priors
  )

  core$duration_workflow_version <-
    "0.1"
  core$prepared <- prepared
  core$fixed_formula <-
    prepared$fixed_formula
  core$fixed_formula_text <-
    prepared$fixed_formula_text
  core$model_matrix_columns <-
    prepared$model_matrix_columns
  core$outcome_unit <-
    prepared$outcome_unit
  core$contains_data <- TRUE
  core$fitting_engine <- "none"
  core$backend_dependency <- "none"
  core$unrestricted_formula <- FALSE
  core$prior_predictive_performed <-
    FALSE

  class(core) <- c(
    "gp3bayes_duration_model_specification",
    class(core)
  )

  core
}

#' Check Duration Prior Predictive Behaviour
#'
#' Simulates positive-duration data from the declared prior specification and
#' prepared design without fitting a model.
#'
#' @param specification A `gp3bayes_duration_model_specification`.
#' @param draws Number of prior predictive data sets.
#' @param seed Non-negative integer seed.
#' @param plausible_median Optional increasing pair for plausible overall
#'   medians in the prepared outcome unit.
#' @param maximum_q99 Maximum plausible 99th percentile.
#' @param maximum_cv Maximum plausible coefficient of variation.
#' @param maximum_condition_ratio Maximum plausible ratio between condition
#'   medians in either direction.
#' @param maximum_extreme_probability Maximum fraction of prior predictive draws
#'   allowed to violate each criterion.
#'
#' @return A `gp3bayes_duration_prior_predictive_check`.
#'
#' @details
#' Failure requests substantive prior review; it does not select or alter
#' priors automatically.
#'
#' @export
check_duration_prior_predictive <- function(
  specification,
  draws = 500L,
  seed = 1L,
  plausible_median = NULL,
  maximum_q99 = NULL,
  maximum_cv = 5,
  maximum_condition_ratio = 10,
  maximum_extreme_probability = 0.25
) {
  if (!inherits(
    specification,
    "gp3bayes_duration_model_specification"
  )) {
    .gp3b_stop(
      "`specification` must inherit from ",
      "`gp3bayes_duration_model_specification`."
    )
  }

  draws <- .gp3b_assert_integer(
    draws,
    "draws",
    minimum = 50L
  )
  seed <- .gp3b_assert_integer(
    seed,
    "seed",
    minimum = 0L
  )

  baseline <-
    specification$priors$baseline

  if (is.null(plausible_median)) {
    plausible_median <- baseline *
      c(
        0.1,
        10
      )
  }

  plausible_median <-
    as.numeric(
      plausible_median
    )

  if (
    length(plausible_median) != 2L ||
      anyNA(plausible_median) ||
      any(!is.finite(plausible_median)) ||
      any(plausible_median <= 0) ||
      plausible_median[[1L]] >=
        plausible_median[[2L]]
  ) {
    .gp3b_stop(
      "`plausible_median` must be an increasing pair of positive finite values."
    )
  }

  if (is.null(maximum_q99)) {
    maximum_q99 <- baseline * 50
  }

  maximum_q99 <-
    .gp3b_assert_numeric_scalar(
      maximum_q99,
      "maximum_q99",
      lower = 0,
      lower_open = TRUE
    )
  maximum_cv <-
    .gp3b_assert_numeric_scalar(
      maximum_cv,
      "maximum_cv",
      lower = 0,
      lower_open = TRUE
    )
  maximum_condition_ratio <-
    .gp3b_assert_numeric_scalar(
      maximum_condition_ratio,
      "maximum_condition_ratio",
      lower = 1,
      lower_open = TRUE
    )
  maximum_extreme_probability <-
    .gp3b_validate_probability(
      maximum_extreme_probability,
      "maximum_extreme_probability"
    )

  prepared <-
    specification$prepared
  data <- prepared$data
  contract <- prepared$contract
  model_matrix <- stats::model.matrix(
    prepared$fixed_formula,
    data = data
  )

  participant <- factor(
    data[[contract$mappings$participant]]
  )
  participant_index <-
    as.integer(participant)
  participant_count <-
    nlevels(participant)

  item <- NULL
  item_index <- NULL
  item_count <- 0L

  if (!is.null(
    contract$mappings$item
  )) {
    item <- factor(
      data[[contract$mappings$item]]
    )
    item_index <-
      as.integer(item)
    item_count <- nlevels(item)
  }

  condition <- if (
    is.null(contract$mappings$condition)
  ) {
    NULL
  } else {
    data[[contract$mappings$condition]]
  }

  intercept_prior <- .gp3b_prior_row(
    specification$priors,
    "Intercept"
  )
  coefficient_prior <- .gp3b_prior_row(
    specification$priors,
    "b"
  )
  group_prior <- .gp3b_prior_row(
    specification$priors,
    "sd"
  )
  sigma_prior <- .gp3b_prior_row(
    specification$priors,
    "sigma"
  )

  correlation_eta <- NULL

  if (isTRUE(
    contract$random_slope
  )) {
    correlation_eta <- .gp3b_prior_row(
      specification$priors,
      "cor"
    )$shape[[1L]]
  }

  set.seed(seed)

  summary_template <-
    .gp3d_duration_summary(
      rep(1, nrow(data)),
      condition,
      participant,
      item
    )

  summaries <- matrix(
    NA_real_,
    nrow = draws,
    ncol =
      length(summary_template),
    dimnames = list(
      NULL,
      names(summary_template)
    )
  )

  for (draw in seq_len(draws)) {
    intercept <- stats::rnorm(
      1L,
      mean =
        intercept_prior$location[[1L]],
      sd =
        intercept_prior$scale[[1L]]
    )

    coefficient_count <-
      ncol(model_matrix) - 1L
    coefficients <- if (
      coefficient_count > 0L
    ) {
      stats::rnorm(
        coefficient_count,
        mean =
          coefficient_prior$location[[1L]],
        sd =
          coefficient_prior$scale[[1L]]
      )
    } else {
      numeric()
    }

    linear_predictor <- rep(
      intercept,
      nrow(data)
    )

    if (coefficient_count > 0L) {
      linear_predictor <-
        linear_predictor +
        as.numeric(
          model_matrix[
            ,
            -1L,
            drop = FALSE
          ] %*% coefficients
        )
    }

    participant_intercept_sd <-
      .gp3b_half_student_t(
        1L,
        df =
          group_prior$df[[1L]],
        scale =
          group_prior$scale[[1L]]
      )
    participant_slope_sd <-
      if (
        isTRUE(
          contract$random_slope
        )
      ) {
        .gp3b_half_student_t(
          1L,
          df =
            group_prior$df[[1L]],
          scale =
            group_prior$scale[[1L]]
        )
      } else {
        0
      }

    z_intercept <- stats::rnorm(
      participant_count
    )
    z_slope <- stats::rnorm(
      participant_count
    )

    correlation <- if (
      isTRUE(
        contract$random_slope
      )
    ) {
      transformed <- stats::rbeta(
        1L,
        shape1 = correlation_eta,
        shape2 = correlation_eta
      )
      2 * transformed - 1
    } else {
      0
    }

    participant_intercept <-
      participant_intercept_sd *
      z_intercept
    participant_slope <-
      participant_slope_sd * (
        correlation * z_intercept +
          sqrt(
            1 - correlation^2
          ) * z_slope
      )

    linear_predictor <-
      linear_predictor +
      participant_intercept[
        participant_index
      ]

    if (
      isTRUE(
        contract$random_slope
      )
    ) {
      linear_predictor <-
        linear_predictor +
        participant_slope[
          participant_index
        ] * condition
    }

    if (item_count > 0L) {
      item_effect_sd <-
        .gp3b_half_student_t(
          1L,
          df =
            group_prior$df[[1L]],
          scale =
            group_prior$scale[[1L]]
        )
      item_effect <- stats::rnorm(
        item_count,
        mean = 0,
        sd = item_effect_sd
      )
      linear_predictor <-
        linear_predictor +
        item_effect[item_index]
    }

    sigma <- .gp3b_half_student_t(
      1L,
      df =
        sigma_prior$df[[1L]],
      scale =
        sigma_prior$scale[[1L]]
    )

    y <- stats::rlnorm(
      nrow(data),
      meanlog =
        linear_predictor,
      sdlog = sigma
    )

    summaries[
      draw,
      ] <- .gp3d_duration_summary(
        y,
        condition,
        participant,
        item
      )
  }

  summaries <- as.data.frame(
    summaries,
    stringsAsFactors = FALSE
  )

  median_violation <- mean(
    summaries$median <
      plausible_median[[1L]] |
      summaries$median >
        plausible_median[[2L]]
  )
  q99_violation <- mean(
    summaries$q99 >
      maximum_q99
  )
  cv_values <-
    summaries$coefficient_of_variation
  cv_violation <- if (
    all(is.na(cv_values))
  ) {
    1
  } else {
    mean(
      cv_values > maximum_cv,
      na.rm = TRUE
    )
  }

  condition_violation <- if (
    all(
      is.na(
        summaries$condition_median_ratio
      )
    )
  ) {
    NA_real_
  } else {
    mean(
      summaries$condition_median_ratio >
        maximum_condition_ratio |
        summaries$condition_median_ratio <
          1 /
            maximum_condition_ratio,
      na.rm = TRUE
    )
  }

  nonfinite_violation <- mean(
    summaries$nonfinite_fraction > 0
  )

  checks <- data.frame(
    check = c(
      "overall_median",
      "upper_tail_q99",
      "coefficient_of_variation",
      "condition_median_ratio",
      "nonfinite_predictions"
    ),
    violation_probability = c(
      median_violation,
      q99_violation,
      cv_violation,
      condition_violation,
      nonfinite_violation
    ),
    maximum_probability =
      maximum_extreme_probability,
    status = c(
      if (
        median_violation <=
          maximum_extreme_probability
      ) {
        "pass"
      } else {
        "fail"
      },
      if (
        q99_violation <=
          maximum_extreme_probability
      ) {
        "pass"
      } else {
        "fail"
      },
      if (
        cv_violation <=
          maximum_extreme_probability
      ) {
        "pass"
      } else {
        "fail"
      },
      if (
        is.na(condition_violation)
      ) {
        "not_applicable"
      } else if (
        condition_violation <=
          maximum_extreme_probability
      ) {
        "pass"
      } else {
        "fail"
      },
      if (
        nonfinite_violation <=
          maximum_extreme_probability
      ) {
        "pass"
      } else {
        "fail"
      }
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      check_version = "0.1",
      family = "duration",
      draws = draws,
      seed = seed,
      outcome_unit =
        prepared$outcome_unit,
      summaries = summaries,
      checks = checks,
      thresholds = list(
        plausible_median =
          plausible_median,
        maximum_q99 =
          maximum_q99,
        maximum_cv =
          maximum_cv,
        maximum_condition_ratio =
          maximum_condition_ratio,
        maximum_extreme_probability =
          maximum_extreme_probability
      ),
      adequate = all(
        checks$status %in%
          c(
            "pass",
            "not_applicable"
          )
      ),
      backend = "none",
      fitting_performed = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class = c(
      "gp3bayes_duration_prior_predictive_check",
      "gp3bayes_prior_predictive_check"
    )
  )
}

#' @export
print.gp3bayes_duration_simulation <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_simulation>\n")
  cat(
    "  Rows: ",
    nrow(x$data),
    "\n",
    sep = ""
  )
  cat(
    "  Participants: ",
    x$design$n_participants,
    "\n",
    sep = ""
  )
  cat(
    "  Outcome unit: ",
    x$design$outcome_unit,
    "\n",
    sep = ""
  )
  cat("  Strictly positive: TRUE\n")
  cat("  Censored: FALSE\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_duration_prepared <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_prepared>\n")
  cat(
    "  Analysis rows: ",
    x$n_analysis_rows,
    "\n",
    sep = ""
  )
  cat(
    "  Rows removed: ",
    x$rows_removed,
    "\n",
    sep = ""
  )
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
  cat(
    "  Readiness status: ",
    x$audit$status,
    "\n",
    sep = ""
  )
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_duration_model_specification <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_model_specification>\n")
  cat(
    "  Formula: ",
    x$formula_text,
    "\n",
    sep = ""
  )
  cat("  Family: lognormal\n")
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
  cat("  Fitting engine: none\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_duration_prior_predictive_check <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_prior_predictive_check>\n")
  cat(
    "  Draws: ",
    x$draws,
    "\n",
    sep = ""
  )
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
  cat(
    "  Prior predictive adequate: ",
    x$adequate,
    "\n",
    sep = ""
  )
  cat("  Fit performed: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}

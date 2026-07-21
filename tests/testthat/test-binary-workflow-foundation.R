
make_binary_foundation_simulation <- function(seed = 2026) {
  simulate_hierarchical_binary_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd = 0.25,
    seed = seed
  )
}

make_binary_foundation_contract <- function(
  random_slope = TRUE,
  include_item = TRUE,
  include_condition = TRUE
) {
  create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = if (include_item) "item_id" else NULL,
    trial_col = "trial_id",
    condition_col = if (include_condition) "condition" else NULL,
    predictors = c(
      "participant_covariate",
      "trial_covariate"
    ),
    interaction = if (include_condition) {
      c(
        "condition",
        "participant_covariate"
      )
    } else {
      NULL
    },
    random_slope = random_slope && include_condition
  )
}

make_binary_foundation_prepared <- function(
  seed = 2026,
  random_slope = TRUE,
  include_item = TRUE,
  include_condition = TRUE
) {
  simulation <- make_binary_foundation_simulation(seed)
  contract <- make_binary_foundation_contract(
    random_slope = random_slope,
    include_item = include_item,
    include_condition = include_condition
  )

  prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = if (include_condition) {
      c(
        "control",
        "treatment"
      )
    } else {
      NULL
    }
  )
}

test_that("binary simulation is deterministic and records truth", {
  first <- make_binary_foundation_simulation(2026)
  second <- make_binary_foundation_simulation(2026)

  expect_s3_class(
    first,
    "gp3bayes_binary_simulation"
  )
  expect_identical(
    first$data,
    second$data
  )
  expect_identical(
    first$truth,
    second$truth
  )
  expect_identical(
    first$random_effects,
    second$random_effects
  )
  expect_equal(
    nrow(first$data),
    96L
  )
  expect_equal(
    length(unique(first$data$participant_id)),
    12L
  )
  expect_equal(
    length(unique(first$data$item_id)),
    6L
  )
  expect_identical(
    levels(first$data$condition),
    c(
      "control",
      "treatment"
    )
  )
  expect_true(
    all(first$data$selected %in% c(0L, 1L))
  )
  expect_true(
    all(first$data$true_probability > 0)
  )
  expect_true(
    all(first$data$true_probability < 1)
  )
  expect_identical(
    first$truth$seed,
    2026L
  )
  expect_equal(
    first$truth$condition_coding,
    c(
      control = -0.5,
      treatment = 0.5
    )
  )
})

test_that("binary simulation supports a design without items or slopes", {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 10,
    trials_per_participant = 6,
    include_items = FALSE,
    random_slope_sd = 0,
    seed = 18
  )

  expect_false(
    "item_id" %in% names(simulation$data)
  )
  expect_null(
    simulation$random_effects$item
  )
  expect_identical(
    simulation$design$n_items,
    0L
  )
  expect_false(
    simulation$design$random_slope
  )
})

test_that("binary simulation validates design and correlation arguments", {
  expect_error(
    simulate_hierarchical_binary_data(
      n_participants = 1
    ),
    "n_participants"
  )

  expect_error(
    simulate_hierarchical_binary_data(
      trials_per_participant = 1
    ),
    "trials_per_participant"
  )

  expect_error(
    simulate_hierarchical_binary_data(
      participant_sd = -1
    ),
    "participant_sd"
  )

  expect_error(
    simulate_hierarchical_binary_data(
      random_slope_cor = 1
    ),
    "random_slope_cor"
  )

  expect_error(
    simulate_hierarchical_binary_data(
      condition_probability = 0
    ),
    "condition_probability"
  )
})

test_that("binary preparation codes the outcome and condition explicitly", {
  simulation <- make_binary_foundation_simulation()
  contract <- make_binary_foundation_contract()

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c(
      "control",
      "treatment"
    ),
    scale_predictors = c(
      "participant_covariate",
      "trial_covariate"
    )
  )

  expect_s3_class(
    prepared,
    "gp3bayes_binary_prepared"
  )
  expect_true(
    prepared$audit$ready
  )
  expect_true(
    prepared$audit$status %in%
      c(
        "ready",
        "ready_with_warnings"
      )
  )
  expect_true(
    all(prepared$data$selected %in% c(0L, 1L))
  )
  expect_equal(
    sort(unique(prepared$data$condition)),
    c(-0.5, 0.5)
  )
  expect_equal(
    mean(prepared$data$participant_covariate),
    0,
    tolerance = 1e-12
  )
  expect_equal(
    stats::sd(prepared$data$participant_covariate),
    1,
    tolerance = 1e-12
  )
  expect_equal(
    mean(prepared$data$trial_covariate),
    0,
    tolerance = 1e-12
  )
  expect_equal(
    stats::sd(prepared$data$trial_covariate),
    1,
    tolerance = 1e-12
  )
  expect_identical(
    prepared$n_input_rows,
    96L
  )
  expect_identical(
    prepared$n_analysis_rows,
    96L
  )
  expect_identical(
    prepared$rows_removed,
    0L
  )
  expect_true(
    prepared$contains_data
  )
  expect_identical(
    prepared$backend,
    "none"
  )
  expect_false(
    prepared$fit_performed
  )
  expect_true(
    all(
      c(
        "(Intercept)",
        "condition",
        "participant_covariate",
        "trial_covariate",
        "condition:participant_covariate"
      ) %in%
        prepared$model_matrix_columns
    )
  )
})

test_that("labelled outcomes require and preserve an explicit mapping", {
  simulation <- make_binary_foundation_simulation()
  simulation$data$selected <- factor(
    ifelse(
      simulation$data$selected == 1,
      "yes",
      "no"
    ),
    levels = c(
      "no",
      "yes"
    )
  )
  contract <- make_binary_foundation_contract()

  expect_error(
    prepare_hierarchical_binary_data(
      simulation$data,
      contract,
      condition_levels = c(
        "control",
        "treatment"
      )
    ),
    "outcome_mapping"
  )

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    outcome_mapping = c(
      no = 0,
      yes = 1
    ),
    condition_levels = c(
      "control",
      "treatment"
    )
  )

  expect_true(
    all(prepared$data$selected %in% c(0L, 1L))
  )
  expect_equal(
    prepared$transformations$outcome$mapping,
    c(
      no = 0,
      yes = 1
    )
  )
})

test_that("missing-row decisions are explicit and reproducible", {
  simulation <- make_binary_foundation_simulation()
  simulation$data$trial_covariate[[1L]] <- NA_real_
  contract <- make_binary_foundation_contract()

  expect_error(
    prepare_hierarchical_binary_data(
      simulation$data,
      contract,
      condition_levels = c(
        "control",
        "treatment"
      ),
      missing = "error"
    ),
    "Missing values"
  )

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c(
      "control",
      "treatment"
    ),
    missing = "drop"
  )

  expect_identical(
    prepared$n_input_rows,
    96L
  )
  expect_identical(
    prepared$n_analysis_rows,
    95L
  )
  expect_identical(
    prepared$rows_removed,
    1L
  )
  expect_identical(
    prepared$transformations$missing$dropped_row_positions,
    1L
  )
  expect_identical(
    prepared$transformations$missing$action,
    "drop"
  )
})

test_that("preparation rejects undeclared scaling and non-binary contracts", {
  simulation <- make_binary_foundation_simulation()
  contract <- make_binary_foundation_contract()

  expect_error(
    prepare_hierarchical_binary_data(
      simulation$data,
      contract,
      condition_levels = c(
        "control",
        "treatment"
      ),
      scale_predictors = "true_probability"
    ),
    "Undeclared"
  )

  duration_contract <- create_model_contract(
    family = "duration",
    outcome_col = "true_probability",
    participant_col = "participant_id",
    outcome_unit = "probability_unit"
  )

  expect_error(
    prepare_hierarchical_binary_data(
      simulation$data,
      duration_contract
    ),
    "binary model contract"
  )
})

test_that("binary specification extends the validated core without fitting", {
  prepared <- make_binary_foundation_prepared()

  specification <- specify_binary_model(
    prepared,
    baseline = 0.35,
    intercept_scale = 1.25,
    coefficient_scale = 0.7,
    group_sd_scale = 0.8,
    correlation_eta = 3,
    student_df = 4
  )

  expect_s3_class(
    specification,
    "gp3bayes_binary_model_specification"
  )
  expect_s3_class(
    specification,
    "gp3bayes_model_specification"
  )
  expect_identical(
    specification$family,
    "binary"
  )
  expect_identical(
    specification$prepared,
    prepared
  )
  expect_s3_class(
    specification$formula,
    "formula"
  )
  expect_s3_class(
    specification$fixed_formula,
    "formula"
  )
  expect_match(
    specification$formula_text,
    "(1 + condition | participant_id)",
    fixed = TRUE
  )
  expect_match(
    specification$formula_text,
    "(1 | item_id)",
    fixed = TRUE
  )
  expect_identical(
    specification$priors$baseline,
    0.35
  )
  expect_equal(
    specification$priors$transformed_baseline,
    stats::qlogis(0.35)
  )
  expect_identical(
    specification$fitting_engine,
    "none"
  )
  expect_identical(
    specification$backend_dependency,
    "none"
  )
  expect_false(
    specification$unrestricted_formula
  )
  expect_false(
    specification$fit_performed
  )
  expect_false(
    specification$prior_predictive_performed
  )
})

test_that("prior predictive checks are deterministic and backend independent", {
  prepared <- make_binary_foundation_prepared()
  specification <- specify_binary_model(
    prepared,
    baseline = 0.35
  )

  first <- check_binary_prior_predictive(
    specification,
    draws = 60,
    seed = 2027
  )
  second <- check_binary_prior_predictive(
    specification,
    draws = 60,
    seed = 2027
  )

  expect_s3_class(
    first,
    "gp3bayes_binary_prior_predictive_check"
  )
  expect_s3_class(
    first,
    "gp3bayes_prior_predictive_check"
  )
  expect_identical(
    first$summaries,
    second$summaries
  )
  expect_identical(
    first$checks,
    second$checks
  )
  expect_identical(
    first$draws,
    60L
  )
  expect_identical(
    first$seed,
    2027L
  )
  expect_identical(
    first$backend,
    "none"
  )
  expect_false(
    first$fitting_performed
  )
  expect_true(
    all(
      c(
        "overall_rate",
        "condition_low_rate",
        "condition_high_rate",
        "condition_rate_contrast",
        "participant_rate_sd",
        "item_rate_sd",
        "participant_all_zero",
        "participant_all_one",
        "probability_below_boundary",
        "probability_above_boundary"
      ) %in%
        names(first$summaries)
    )
  )
  expect_true(
    all(
      first$checks$status %in%
        c(
          "pass",
          "fail",
          "not_applicable"
        )
    )
  )
  expect_type(
    first$adequate,
    "logical"
  )
  expect_length(
    first$adequate,
    1L
  )
})

test_that("condition checks become not applicable without a condition", {
  prepared <- make_binary_foundation_prepared(
    random_slope = FALSE,
    include_item = FALSE,
    include_condition = FALSE
  )
  specification <- specify_binary_model(
    prepared,
    baseline = 0.35
  )
  check <- check_binary_prior_predictive(
    specification,
    draws = 50,
    seed = 2027
  )

  condition_rows <- check$checks$check %in%
    c(
      "condition_rates",
      "condition_contrast"
    )

  expect_true(
    all(
      check$checks$status[condition_rows] ==
        "not_applicable"
    )
  )
  expect_true(
    all(
      is.na(check$checks$probability[condition_rows])
    )
  )
})

test_that("prior predictive controls are validated", {
  prepared <- make_binary_foundation_prepared()
  specification <- specify_binary_model(prepared)

  expect_error(
    check_binary_prior_predictive(
      specification,
      draws = 49
    ),
    "draws"
  )

  expect_error(
    check_binary_prior_predictive(
      specification,
      plausible_rate = c(0.9, 0.1)
    ),
    "plausible_rate"
  )

  expect_error(
    check_binary_prior_predictive(
      specification,
      maximum_extreme_probability = 2
    ),
    "maximum_extreme_probability"
  )
})

test_that("binary workflow print methods are concise", {
  simulation <- make_binary_foundation_simulation()
  prepared <- make_binary_foundation_prepared()
  specification <- specify_binary_model(prepared)
  check <- check_binary_prior_predictive(
    specification,
    draws = 50,
    seed = 2027
  )

  expect_output(
    printed_simulation <- print(simulation),
    "<gp3bayes_binary_simulation>",
    fixed = TRUE
  )
  expect_identical(
    printed_simulation,
    simulation
  )

  expect_output(
    printed_prepared <- print(prepared),
    "Fit performed: FALSE",
    fixed = TRUE
  )
  expect_identical(
    printed_prepared,
    prepared
  )

  expect_output(
    printed_specification <- print(specification),
    "Fitting engine: none",
    fixed = TRUE
  )
  expect_identical(
    printed_specification,
    specification
  )

  expect_output(
    printed_check <- print(check),
    "Fit performed: FALSE",
    fixed = TRUE
  )
  expect_identical(
    printed_check,
    check
  )
})

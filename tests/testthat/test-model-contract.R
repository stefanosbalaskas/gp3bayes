test_that("binary model contracts preserve approved metadata", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "stimulus_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = "age_z",
    interaction = c("condition", "age_z"),
    random_slope = TRUE
  )

  expect_s3_class(contract, "gp3bayes_model_contract")
  expect_identical(contract$family, "binary")
  expect_identical(contract$model_family, "hierarchical_binary")
  expect_identical(contract$likelihood, "Bernoulli")
  expect_identical(contract$link, "logit")
  expect_identical(contract$mappings$outcome, "selected")
  expect_identical(contract$interaction, c("condition", "age_z"))
  expect_true(contract$random_slope)
  expect_null(contract$outcome_unit)
})

test_that("duration contracts require and preserve the outcome unit", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    trial_col = "trial_id",
    condition_col = "condition",
    outcome_unit = "milliseconds"
  )

  expect_s3_class(contract, "gp3bayes_model_contract")
  expect_identical(
    contract$model_family,
    "hierarchical_lognormal_duration"
  )
  expect_identical(contract$likelihood, "lognormal")
  expect_identical(contract$outcome_unit, "milliseconds")
})

test_that("unsupported model families are rejected", {
  expect_error(
    create_model_contract(
      family = "count",
      outcome_col = "events",
      participant_col = "participant_id"
    ),
    "Unsupported `family`"
  )
})

test_that("required column mappings are validated", {
  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "",
      participant_col = "participant_id"
    ),
    "`outcome_col` must be one non-empty character value"
  )

  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = NA_character_
    ),
    "`participant_col` must be one non-empty character value"
  )
})

test_that("column mappings and predictors must be unique", {
  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = "participant_id",
      predictors = "selected"
    ),
    "Column mappings and predictors must be unique"
  )
})

test_that("interactions are restricted to two declared variables", {
  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = "participant_id",
      predictors = c("age_z", "score_z"),
      interaction = c("age_z", "score_z", "condition")
    ),
    "`interaction` must contain exactly two declared variables"
  )

  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = "participant_id",
      predictors = "age_z",
      interaction = c("age_z", "undeclared")
    ),
    "Every interaction variable must be declared"
  )
})

test_that("random slopes require a declared condition", {
  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = "participant_id",
      random_slope = TRUE
    ),
    "`condition_col` must be supplied"
  )
})

test_that("outcome-unit rules are family-specific", {
  expect_error(
    create_model_contract(
      family = "binary",
      outcome_col = "selected",
      participant_col = "participant_id",
      outcome_unit = "probability"
    ),
    "`outcome_unit` must be NULL for the binary family"
  )

  expect_error(
    create_model_contract(
      family = "duration",
      outcome_col = "response_time",
      participant_col = "participant_id"
    ),
    "`outcome_unit` must be one non-empty character value"
  )
})

test_that("contracts expose the complete approved methodological record", {
  binary_contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  duration_contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  required_fields <- c(
    "repeated_measures_structure",
    "supported_predictors",
    "supported_interactions",
    "supported_offsets_or_exposures",
    "supported_censoring",
    "prior_rationale",
    "interpretation_boundaries"
  )

  contracts <- list(
    binary_contract,
    duration_contract
  )

  for (contract in contracts) {
    expect_true(all(required_fields %in% names(contract)))
    expect_length(contract$repeated_measures_structure, 3L)
    expect_length(contract$supported_predictors, 3L)
    expect_length(contract$supported_interactions, 1L)
    expect_identical(
      contract$supported_offsets_or_exposures,
      "Not supported"
    )
    expect_length(contract$prior_rationale, 4L)
    expect_true(length(contract$interpretation_boundaries) >= 3L)
  }

  expect_identical(
    binary_contract$supported_censoring,
    "Not applicable"
  )

  expect_match(
    duration_contract$supported_censoring,
    "uncensored",
    fixed = TRUE
  )
})

test_that("printing is concise and returns the contract invisibly", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  expect_output(
    printed <- print(contract),
    "<gp3bayes_model_contract>",
    fixed = TRUE
  )

  expect_identical(printed, contract)
})

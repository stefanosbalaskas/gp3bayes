
make_binary_readiness_data <- function() {
  data.frame(
    participant_id = rep(
      paste0("p", 1:4),
      each = 4
    ),
    stimulus_id = rep(
      paste0("s", 1:4),
      times = 4
    ),
    trial_id = rep(
      1:4,
      times = 4
    ),
    condition = factor(
      rep(
        c("control", "treatment"),
        times = 8
      ),
      levels = c(
        "control",
        "treatment"
      )
    ),
    trial_order = rep(
      1:4,
      times = 4
    ),
    age_z = rep(
      c(-1, -0.5, 0.5, 1),
      each = 4
    ),
    selected = rep(
      c(0, 1, 1, 0),
      times = 4
    ),
    stringsAsFactors = FALSE
  )
}

make_duration_readiness_data <- function() {
  data <- make_binary_readiness_data()

  data$response_time <- c(
    410, 520, 470, 610,
    390, 505, 455, 590,
    430, 540, 485, 625,
    405, 515, 465, 600
  )

  data
}

readiness_status_for <- function(audit, check_id) {
  location <- match(
    check_id,
    audit$checks$check_id
  )

  audit$checks$status[[location]]
}

test_that("a complete binary data set is ready", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "stimulus_id",
    trial_col = "trial_id",
    condition_col = "condition",
    time_col = "trial_order",
    predictors = "age_z",
    interaction = c("condition", "age_z"),
    random_slope = TRUE
  )

  audit <- audit_model_readiness(
    make_binary_readiness_data(),
    contract
  )

  expect_s3_class(
    audit,
    "gp3bayes_readiness_audit"
  )
  expect_true(audit$ready)
  expect_identical(
    audit$status,
    "ready"
  )
  expect_identical(
    unname(audit$status_counts[["fail"]]),
    0L
  )
  expect_identical(
    readiness_status_for(
      audit,
      "outcome_values"
    ),
    "pass"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "random_slope_support"
    ),
    "pass"
  )
  expect_false(
    "data" %in% names(audit)
  )
})

test_that("a complete duration data set is ready", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    item_col = "stimulus_id",
    trial_col = "trial_id",
    condition_col = "condition",
    time_col = "trial_order",
    predictors = "age_z",
    outcome_unit = "milliseconds"
  )

  audit <- audit_model_readiness(
    make_duration_readiness_data(),
    contract
  )

  expect_true(audit$ready)
  expect_identical(
    audit$status,
    "ready"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "duration_finite"
    ),
    "pass"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "duration_positive"
    ),
    "pass"
  )
})

test_that("invalid input objects are rejected", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  expect_error(
    audit_model_readiness(
      matrix(
        c(0, 1),
        ncol = 1
      ),
      contract
    ),
    "`data` must be a data frame"
  )

  expect_error(
    audit_model_readiness(
      data.frame(
        selected = c(0, 1),
        participant_id = c("p1", "p1")
      ),
      list()
    ),
    "`contract` must inherit"
  )
})

test_that("missing declared columns produce a failed audit", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data.frame(
      selected = c(0, 1)
    ),
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    audit$status,
    "not_ready"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "required_columns"
    ),
    "fail"
  )
  expect_identical(
    audit$columns$missing,
    "participant_id"
  )
})

test_that("missing analysis values produce a failed audit", {
  data <- make_binary_readiness_data()
  data$selected[[1L]] <- NA_real_

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    trial_col = "trial_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "analysis_missingness"
    ),
    "fail"
  )
})

test_that("binary outcomes reject invalid values", {
  data <- make_binary_readiness_data()
  data$selected[[1L]] <- 2

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "outcome_values"
    ),
    "fail"
  )
})

test_that("binary outcomes require both classes", {
  data <- make_binary_readiness_data()
  data$selected <- 1

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "outcome_support"
    ),
    "fail"
  )
})

test_that("duration outcomes must be finite and positive", {
  data <- make_duration_readiness_data()
  data$response_time[[1L]] <- 0
  data$response_time[[2L]] <- Inf

  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "duration_finite"
    ),
    "fail"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "duration_positive"
    ),
    "fail"
  )
})

test_that("a repeated-measures structure is required", {
  data <- data.frame(
    participant_id = c(
      "p1",
      "p2"
    ),
    selected = c(
      0,
      1
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "repeated_measurement"
    ),
    "fail"
  )
})

test_that("participant singletons produce a warning when repeats exist", {
  data <- data.frame(
    participant_id = c(
      "p1",
      "p1",
      "p2"
    ),
    selected = c(
      0,
      1,
      0
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_true(audit$ready)
  expect_identical(
    audit$status,
    "ready_with_warnings"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "repeated_measurement"
    ),
    "warn"
  )
})

test_that("duplicated participant-trial keys fail", {
  data <- data.frame(
    participant_id = c(
      "p1",
      "p1",
      "p2",
      "p2"
    ),
    trial_id = c(
      1,
      1,
      1,
      2
    ),
    selected = c(
      0,
      1,
      0,
      1
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    trial_col = "trial_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "trial_key"
    ),
    "fail"
  )
})

test_that("random slopes require within-participant condition variation", {
  data <- make_binary_readiness_data()

  data$condition[
    data$participant_id == "p1"
  ] <- "control"

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    condition_col = "condition",
    random_slope = TRUE
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "random_slope_support"
    ),
    "fail"
  )
})

test_that("invalid time and predictor structures are reported", {
  data <- make_binary_readiness_data()

  data$bad_time <- rep(
    letters[1:4],
    times = 4
  )
  data$constant <- 1
  data$bad_list <- I(
    as.list(
      seq_len(nrow(data))
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    time_col = "bad_time",
    predictors = c(
      "constant",
      "bad_list"
    )
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_false(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "time_type"
    ),
    "fail"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "predictor_types"
    ),
    "fail"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "predictor_variation"
    ),
    "fail"
  )
})

test_that("weak item crossing produces a warning", {
  data <- data.frame(
    participant_id = c(
      "p1",
      "p1",
      "p2",
      "p2"
    ),
    stimulus_id = c(
      "s1",
      "s1",
      "s2",
      "s2"
    ),
    selected = c(
      0,
      1,
      0,
      1
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "stimulus_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_true(audit$ready)
  expect_identical(
    audit$status,
    "ready_with_warnings"
  )
  expect_identical(
    readiness_status_for(
      audit,
      "item_crossing"
    ),
    "warn"
  )
})

test_that("categorical interactions require replicated combinations", {
  data <- data.frame(
    participant_id = rep(
      c("p1", "p2"),
      each = 3
    ),
    condition = c(
      "a",
      "a",
      "b",
      "a",
      "b",
      "b"
    ),
    group = c(
      "x",
      "x",
      "y",
      "x",
      "y",
      "z"
    ),
    selected = c(
      0,
      1,
      0,
      1,
      0,
      1
    )
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    condition_col = "condition",
    predictors = "group",
    interaction = c(
      "condition",
      "group"
    )
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  expect_true(audit$ready)
  expect_identical(
    readiness_status_for(
      audit,
      "interaction_support"
    ),
    "warn"
  )
})

test_that("printing is concise and returns the audit invisibly", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    make_binary_readiness_data(),
    contract
  )

  expect_output(
    printed <- print(audit),
    "<gp3bayes_readiness_audit>",
    fixed = TRUE
  )

  expect_output(
    print(audit),
    "Ready: TRUE",
    fixed = TRUE
  )

  expect_identical(
    printed,
    audit
  )
})

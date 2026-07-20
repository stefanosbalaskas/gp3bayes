
make_model_specification_data <- function() {
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
    response_time = c(
      410, 520, 470, 610,
      390, 505, 455, 590,
      430, 540, 485, 625,
      405, 515, 465, 600
    ),
    stringsAsFactors = FALSE
  )
}

make_binary_specification_contract <- function(
  random_slope = TRUE
) {
  create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "stimulus_id",
    trial_col = "trial_id",
    condition_col = "condition",
    time_col = "trial_order",
    predictors = "age_z",
    interaction = c(
      "condition",
      "age_z"
    ),
    random_slope = random_slope
  )
}

formula_text_for_test <- function(formula) {
  paste(
    deparse(
      formula,
      width.cutoff = 500L
    ),
    collapse = " "
  )
}

test_that("binary formulas contain all approved terms", {
  contract <- make_binary_specification_contract(
    random_slope = TRUE
  )

  formula <- build_model_formula(contract)
  formula_text <- formula_text_for_test(formula)

  expect_s3_class(
    formula,
    "formula"
  )
  expect_match(
    formula_text,
    "selected ~",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "condition",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "trial_order",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "age_z",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "condition:age_z",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "(1 + condition | participant_id)",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "(1 | stimulus_id)",
    fixed = TRUE
  )
})

test_that("random-intercept formulas omit unrequested structures", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    condition_col = "condition",
    outcome_unit = "milliseconds"
  )

  formula <- build_model_formula(contract)
  formula_text <- formula_text_for_test(formula)

  expect_match(
    formula_text,
    "response_time ~ condition",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "(1 | participant_id)",
    fixed = TRUE
  )
  expect_false(
    grepl(
      "stimulus_id",
      formula_text,
      fixed = TRUE
    )
  )
  expect_false(
    grepl(
      "1 + condition",
      formula_text,
      fixed = TRUE
    )
  )
})

test_that("formula construction supports non-syntactic column names", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected outcome",
    participant_col = "participant id",
    predictors = "age score"
  )

  formula <- build_model_formula(contract)
  formula_text <- formula_text_for_test(formula)

  expect_match(
    formula_text,
    "`selected outcome`",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "`age score`",
    fixed = TRUE
  )
  expect_match(
    formula_text,
    "`participant id`",
    fixed = TRUE
  )
})

test_that("binary prior specifications transform the baseline correctly", {
  contract <- make_binary_specification_contract(
    random_slope = TRUE
  )

  priors <- create_prior_specification(
    contract,
    baseline = 0.25,
    intercept_scale = 1.25,
    coefficient_scale = 0.75,
    group_sd_scale = 0.8,
    correlation_eta = 3,
    student_df = 4
  )

  expect_s3_class(
    priors,
    "gp3bayes_prior_specification"
  )
  expect_equal(
    priors$transformed_baseline,
    stats::qlogis(0.25)
  )
  expect_identical(
    priors$table$parameter_class,
    c(
      "Intercept",
      "b",
      "sd",
      "cor"
    )
  )
  expect_identical(
    priors$table$distribution,
    c(
      "normal",
      "normal",
      "student_t",
      "lkj"
    )
  )
  expect_identical(
    priors$backend,
    "none"
  )
  expect_false(
    priors$executable
  )
  expect_identical(
    validate_prior_specification(
      priors,
      contract
    ),
    priors
  )
})

test_that("binary prior defaults are explicit and complete", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  priors <- create_prior_specification(contract)

  expect_identical(
    priors$baseline,
    0.5
  )
  expect_identical(
    priors$transformed_baseline,
    0
  )
  expect_identical(
    priors$table$parameter_class,
    c(
      "Intercept",
      "b",
      "sd"
    )
  )
  expect_false(
    "cor" %in%
      priors$table$parameter_class
  )
  expect_false(
    "sigma" %in%
      priors$table$parameter_class
  )
})

test_that("duration prior specifications use the log baseline", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  priors <- create_prior_specification(
    contract,
    baseline = 500,
    residual_scale = 0.7
  )

  expect_equal(
    priors$transformed_baseline,
    log(500)
  )
  expect_identical(
    priors$outcome_unit,
    "milliseconds"
  )
  expect_identical(
    priors$table$parameter_class,
    c(
      "Intercept",
      "b",
      "sd",
      "sigma"
    )
  )

  sigma_row <- priors$table[
    priors$table$parameter_class == "sigma",
    ,
    drop = FALSE
  ]

  expect_identical(
    sigma_row$distribution,
    "student_t"
  )
  expect_identical(
    sigma_row$scale,
    0.7
  )
  expect_identical(
    sigma_row$lower,
    0
  )
})

test_that("duration priors require a positive baseline", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  expect_error(
    create_prior_specification(contract),
    "`baseline` must be supplied"
  )

  expect_error(
    create_prior_specification(
      contract,
      baseline = 0
    ),
    "strictly positive"
  )

  expect_error(
    create_prior_specification(
      contract,
      baseline = -500
    ),
    "strictly positive"
  )
})

test_that("family-specific prior arguments are enforced", {
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

  expect_error(
    create_prior_specification(
      binary_contract,
      baseline = 0
    ),
    "strictly between zero and one"
  )

  expect_error(
    create_prior_specification(
      binary_contract,
      baseline = 1
    ),
    "strictly between zero and one"
  )

  expect_error(
    create_prior_specification(
      binary_contract,
      residual_scale = 1
    ),
    "`residual_scale` must be NULL"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      intercept_scale = 0
    ),
    "strictly positive"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      coefficient_scale = 0
    ),
    "strictly positive"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      group_sd_scale = -1
    ),
    "strictly positive"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      residual_scale = 0
    ),
    "strictly positive"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      correlation_eta = 0.5
    ),
    "greater than or equal to one"
  )

  expect_error(
    create_prior_specification(
      duration_contract,
      baseline = 500,
      student_df = 0
    ),
    "strictly positive"
  )
})

test_that("prior validation detects incomplete and duplicated classes", {
  contract <- make_binary_specification_contract(
    random_slope = TRUE
  )

  priors <- create_prior_specification(contract)

  missing_sd <- priors
  missing_sd$table <- missing_sd$table[
    missing_sd$table$parameter_class != "sd",
    ,
    drop = FALSE
  ]

  expect_error(
    validate_prior_specification(
      missing_sd,
      contract
    ),
    "missing: sd"
  )

  duplicated_intercept <- priors
  duplicated_intercept$table <- rbind(
    duplicated_intercept$table,
    duplicated_intercept$table[
      duplicated_intercept$table$parameter_class == "Intercept",
      ,
      drop = FALSE
    ]
  )

  expect_error(
    validate_prior_specification(
      duplicated_intercept,
      contract
    ),
    "must be unique"
  )

  unsupported_class <- priors
  unsupported_class$table$parameter_class[
    unsupported_class$table$parameter_class == "b"
  ] <- "shape"

  expect_error(
    validate_prior_specification(
      unsupported_class,
      contract
    ),
    "incomplete or unsupported"
  )
})

test_that("prior validation detects invalid distributions and scales", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  priors <- create_prior_specification(
    contract,
    baseline = 500
  )

  invalid_distribution <- priors
  invalid_distribution$table$distribution[
    invalid_distribution$table$parameter_class == "sigma"
  ] <- "normal"

  expect_error(
    validate_prior_specification(
      invalid_distribution,
      contract
    ),
    "Incorrect prior distributions"
  )

  invalid_normal_scale <- priors
  invalid_normal_scale$table$scale[
    invalid_normal_scale$table$parameter_class == "b"
  ] <- 0

  expect_error(
    validate_prior_specification(
      invalid_normal_scale,
      contract
    ),
    "strictly positive finite scales"
  )

  invalid_student_scale <- priors
  invalid_student_scale$table$scale[
    invalid_student_scale$table$parameter_class == "sd"
  ] <- -1

  expect_error(
    validate_prior_specification(
      invalid_student_scale,
      contract
    ),
    "Half-Student-t priors"
  )

  invalid_student_df <- priors
  invalid_student_df$table$df[
    invalid_student_df$table$parameter_class == "sigma"
  ] <- 0

  expect_error(
    validate_prior_specification(
      invalid_student_df,
      contract
    ),
    "Half-Student-t priors"
  )
})

test_that("prior validation detects invalid rationale and metadata", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  priors <- create_prior_specification(contract)

  missing_rationale <- priors
  missing_rationale$table$rationale[[1L]] <- ""

  expect_error(
    validate_prior_specification(
      missing_rationale,
      contract
    ),
    "non-empty rationale"
  )

  incorrect_baseline <- priors
  incorrect_baseline$transformed_baseline <- 2

  expect_error(
    validate_prior_specification(
      incorrect_baseline,
      contract
    ),
    "inconsistent"
  )

  executable_prior <- priors
  executable_prior$executable <- TRUE

  expect_error(
    validate_prior_specification(
      executable_prior,
      contract
    ),
    "must be FALSE"
  )

  backend_prior <- priors
  backend_prior$backend <- "brms"

  expect_error(
    validate_prior_specification(
      backend_prior,
      contract
    ),
    "must be \"none\""
  )
})

test_that("LKJ prior validation detects invalid shape and bounds", {
  contract <- make_binary_specification_contract(
    random_slope = TRUE
  )

  priors <- create_prior_specification(contract)

  invalid_shape <- priors
  invalid_shape$table$shape[
    invalid_shape$table$parameter_class == "cor"
  ] <- 0.5

  expect_error(
    validate_prior_specification(
      invalid_shape,
      contract
    ),
    "LKJ priors require"
  )

  invalid_bounds <- priors
  invalid_bounds$table$lower[
    invalid_bounds$table$parameter_class == "cor"
  ] <- 0

  expect_error(
    validate_prior_specification(
      invalid_bounds,
      contract
    ),
    "LKJ priors require"
  )
})

test_that("prior validation detects contract incompatibility", {
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

  priors <- create_prior_specification(
    binary_contract
  )

  expect_error(
    validate_prior_specification(
      priors,
      duration_contract
    ),
    "incompatible with `contract`"
  )
})

test_that("a ready audit produces a complete model specification", {
  data <- make_model_specification_data()

  contract <- make_binary_specification_contract(
    random_slope = TRUE
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  priors <- create_prior_specification(
    contract,
    baseline = 0.5
  )

  specification <- create_model_specification(
    contract,
    audit,
    priors
  )

  expect_true(
    audit$ready
  )
  expect_identical(
    audit$status,
    "ready"
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
    specification$readiness_status,
    "ready"
  )
  expect_identical(
    specification$warning_count,
    0L
  )
  expect_s3_class(
    specification$formula,
    "formula"
  )
  expect_match(
    specification$formula_text,
    "(1 + condition | participant_id)",
    fixed = TRUE
  )
  expect_identical(
    specification$backend,
    "none"
  )
  expect_false(
    specification$fit_performed
  )
  expect_identical(
    specification$contract,
    contract
  )
  expect_identical(
    specification$audit,
    audit
  )
  expect_identical(
    specification$priors,
    priors
  )
})

test_that("ready audits with warnings may proceed", {
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

  priors <- create_prior_specification(contract)

  specification <- create_model_specification(
    contract,
    audit,
    priors
  )

  expect_true(
    audit$ready
  )
  expect_identical(
    audit$status,
    "ready_with_warnings"
  )
  expect_identical(
    specification$readiness_status,
    "ready_with_warnings"
  )
  expect_true(
    specification$warning_count > 0L
  )
})

test_that("failed readiness audits are rejected", {
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

  priors <- create_prior_specification(contract)

  expect_false(
    audit$ready
  )

  expect_error(
    create_model_specification(
      contract,
      audit,
      priors
    ),
    "not ready for model specification"
  )
})

test_that("model specifications reject audit-contract mismatches", {
  data <- make_model_specification_data()

  contract_one <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  contract_two <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    condition_col = "condition"
  )

  audit <- audit_model_readiness(
    data,
    contract_one
  )

  priors <- create_prior_specification(
    contract_two
  )

  expect_error(
    create_model_specification(
      contract_two,
      audit,
      priors
    ),
    "audit.*contract.*must be identical"
  )
})

test_that("invalid specification object types are rejected", {
  expect_error(
    build_model_formula(list()),
    "`contract` must inherit"
  )

  expect_error(
    validate_prior_specification(list()),
    "`priors` must inherit"
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  priors <- create_prior_specification(contract)

  expect_error(
    create_model_specification(
      contract,
      list(),
      priors
    ),
    "`audit` must inherit"
  )
})

test_that("printing prior specifications is concise", {
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "response_time",
    participant_col = "participant_id",
    outcome_unit = "milliseconds"
  )

  priors <- create_prior_specification(
    contract,
    baseline = 500
  )

  expect_output(
    printed <- print(priors),
    "<gp3bayes_prior_specification>",
    fixed = TRUE
  )

  expect_output(
    print(priors),
    "Outcome unit: milliseconds",
    fixed = TRUE
  )

  expect_output(
    print(priors),
    "Parameter classes: Intercept, b, sd, sigma",
    fixed = TRUE
  )

  expect_output(
    print(priors),
    "Executable: FALSE",
    fixed = TRUE
  )

  expect_identical(
    printed,
    priors
  )
})

test_that("printing model specifications is concise", {
  data <- make_model_specification_data()

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id"
  )

  audit <- audit_model_readiness(
    data,
    contract
  )

  priors <- create_prior_specification(contract)

  specification <- create_model_specification(
    contract,
    audit,
    priors
  )

  expect_output(
    printed <- print(specification),
    "<gp3bayes_model_specification>",
    fixed = TRUE
  )

  expect_output(
    print(specification),
    "Readiness status: ready",
    fixed = TRUE
  )

  expect_output(
    print(specification),
    "Prior classes: Intercept, b, sd",
    fixed = TRUE
  )

  expect_output(
    print(specification),
    "Fit performed: FALSE",
    fixed = TRUE
  )

  expect_identical(
    printed,
    specification
  )
})

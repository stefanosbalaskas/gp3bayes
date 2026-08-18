# Regression tests for gp3bayes 0.5 advanced pupillometry repairs.
#
# These tests are translation/structure only.
# They do not compile or sample Stan models.

test_that("advanced distributional translation uses valid brms family and sigma formula", {
  skip_if_not_installed("brms")

  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 12,
    seed = 5101
  )

  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    family = "gaussian",
    residual_scale = "time",
    autocorrelation = "ar1"
  )

  tr <- translate_advanced_pupil_model_to_brms(spec)

  expect_s3_class(
    tr,
    "gp3bayes_pupil_advanced_brms_specification"
  )

  expect_s3_class(
    tr$family,
    "brmsfamily"
  )

  expect_identical(
    tr$family$family,
    "gaussian"
  )

  txt <- paste(
    capture.output(print(tr$formula)),
    collapse = " "
  )

  expect_match(
    txt,
    "sigma ~",
    fixed = TRUE
  )
})

test_that("advanced Student-t translation uses a valid brms family", {
  skip_if_not_installed("brms")

  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 12,
    seed = 5102
  )

  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    family = "student",
    residual_scale = "time",
    autocorrelation = "none"
  )

  tr <- translate_advanced_pupil_model_to_brms(spec)

  expect_identical(
    tr$family$family,
    "student"
  )
})

test_that("binocular default basis adapts to sparse temporal support", {
  sim <- simulate_binocular_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 9,
    time_range = c(0, 800),
    seed = 5103
  )

  prep <- prepare_binocular_pupil_timecourse(
    sim$data
  )

  spec <- specify_binocular_pupil_model(
    prep
  )

  expect_identical(
    spec$smooth_basis_dimension_requested,
    10L
  )

  expect_identical(
    spec$smooth_basis_dimension_effective,
    8L
  )

  expect_identical(
    spec$smooth_basis_dimension,
    8L
  )

  expect_identical(
    spec$smooth_basis_support,
    8L
  )

  expect_true(
    spec$smooth_basis_adjusted
  )

  expect_error(
    specify_binocular_pupil_model(
      prep,
      smooth_basis_dimension = 10L
    ),
    "exceeds the governed support",
    fixed = TRUE
  )
})

test_that("sparse-grid binocular model translates through brms", {
  skip_if_not_installed("brms")

  sim <- simulate_binocular_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 9,
    time_range = c(0, 800),
    seed = 5104
  )

  prep <- prepare_binocular_pupil_timecourse(
    sim$data
  )

  spec <- specify_binocular_pupil_model(
    prep
  )

  tr <- suppressWarnings(
    translate_binocular_pupil_model_to_brms(
      spec
    )
  )

  expect_s3_class(
    tr,
    "gp3bayes_binocular_brms_specification"
  )

  expect_length(
    tr$family,
    2L
  )
})

test_that("response-shape translation uses brms-safe nonlinear parameters", {
  skip_if_not_installed("brms")

  sim <- simulate_pupil_response_shape(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 13,
    seed = 5105
  )

  spec <- specify_pupil_response_shape_model(
    sim$data
  )

  tr <- translate_pupil_response_shape_to_brms(
    spec
  )

  expect_s3_class(
    tr,
    "gp3bayes_pupil_response_shape_brms_specification"
  )

  txt <- paste(
    capture.output(print(tr$formula)),
    collapse = " "
  )

  expect_match(txt, "logAmplitude", fixed = TRUE)
  expect_match(txt, "logRise", fixed = TRUE)
  expect_match(txt, "logDuration", fixed = TRUE)
  expect_match(txt, "logDecay", fixed = TRUE)

  expect_false(
    grepl("log_amplitude", txt, fixed = TRUE)
  )

  expect_false(
    grepl("log_rise", txt, fixed = TRUE)
  )

  expect_false(
    grepl("log_duration", txt, fixed = TRUE)
  )

  expect_false(
    grepl("log_decay", txt, fixed = TRUE)
  )
})

test_that("response-shape priors validate through brms", {
  skip_if_not_installed("brms")

  sim <- simulate_pupil_response_shape(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 13,
    seed = 5106
  )

  spec <- specify_pupil_response_shape_model(
    sim$data
  )

  tr <- translate_pupil_response_shape_to_brms(
    spec
  )

  validated <- brms::validate_prior(
    prior = tr$priors,
    formula = tr$formula,
    data = tr$data,
    family = tr$family
  )

  expect_s3_class(
    validated,
    "brmsprior"
  )
})

# 0.5 regression: advanced sparse smooth basis governance
test_that("advanced default smooth basis adapts to sparse temporal support", {
  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 8,
    time_range = c(0, 700),
    seed = 5201
  )

  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    autocorrelation = "none"
  )

  expect_identical(
    spec$smooth_basis_dimension_requested,
    10L
  )

  expect_identical(
    spec$smooth_basis_dimension_effective,
    7L
  )

  expect_identical(
    spec$smooth_basis_dimension,
    7L
  )

  expect_identical(
    spec$smooth_basis_support,
    7L
  )

  expect_true(
    spec$smooth_basis_adjusted
  )

  expect_error(
    specify_advanced_pupil_timecourse_model(
      sim$data,
      smooth_basis_dimension = 10L,
      autocorrelation = "none"
    ),
    "exceeds the governed temporal support",
    fixed = TRUE
  )
})

# 0.5 regression: measurement-error names remain explicit
test_that("common measurement-error declarations retain covariate names", {
  mm <- create_pupil_measurement_model(
    baseline_error = "baseline_se",
    luminance_error = "luminance_se"
  )

  expect_identical(
    names(mm$covariate_errors),
    c("baseline", "luminance")
  )

  tab <- pupil_measurement_uncertainty_table(mm)

  expect_equal(nrow(tab), 2L)
  expect_identical(
    tab$variable,
    c("baseline", "luminance")
  )
  expect_identical(
    tab$error_column,
    c("baseline_se", "luminance_se")
  )
  expect_true(all(tab$role == "predictor"))
})

# 0.5 regression: binocular multivariate priors are response-scoped
test_that("binocular multivariate priors validate and retain response scope", {
  skip_if_not_installed("brms")

  sim <- simulate_binocular_pupil_timecourse(
    n_participants = 6,
    trials_per_participant = 2,
    time_points = 9,
    seed = 5301
  )

  prepared <- prepare_binocular_pupil_timecourse(sim$data)

  spec <- specify_binocular_pupil_model(
    prepared,
    family = "gaussian",
    temporal_structure = "linear",
    residual_correlation = TRUE
  )

  tr <- translate_binocular_pupil_model_to_brms(spec)

  validated <- brms::validate_prior(
    prior = tr$priors,
    formula = tr$formula,
    data = tr$data,
    family = tr$family
  )

  expect_s3_class(validated, "brmsprior")

  prior_df <- as.data.frame(tr$priors)

  response_classes <- prior_df$class %in% c(
    "Intercept", "b", "sd", "sds", "sdgp", "sigma", "nu"
  )

  expect_true(any(response_classes))
  expect_true(
    all(
      !is.na(prior_df$resp[response_classes]) &
        nzchar(prior_df$resp[response_classes])
    )
  )

  expect_true(any(prior_df$class == "rescor"))

  rescor_rows <- prior_df$class == "rescor"

  expect_true(
    all(
      is.na(prior_df$resp[rescor_rows]) |
        !nzchar(prior_df$resp[rescor_rows])
    )
  )
})

testthat::test_that("binocular correlation preserves posterior draw metadata", {
  body_text <- paste(
    deparse(
      body(pupil_binocular_correlation),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  testthat::expect_true(
    grepl(
      "posterior::subset_draws(draws, variable = vars)",
      body_text,
      fixed = TRUE
    )
  )

  testthat::expect_true(
    grepl(
      "posterior::as_draws_matrix",
      body_text,
      fixed = TRUE
    )
  )

  testthat::expect_false(
    grepl(
      "as.matrix(draws[, vars, drop = FALSE])",
      body_text,
      fixed = TRUE
    )
  )
})

testthat::test_that("binocular correlation excludes internal Cholesky parameters", {
  body_text <- paste(
    deparse(
      body(pupil_binocular_correlation),
      width.cutoff = 500L
    ),
    collapse = "\n"
  )

  testthat::expect_true(
    grepl(
      'startsWith(names(draws), "rescor__")',
      body_text,
      fixed = TRUE
    )
  )

  candidate_names <- c(
    "rescor__pupilleft__pupilright",
    "Lrescor[1,1]",
    "Lrescor[2,1]",
    "Lrescor[1,2]",
    "Lrescor[2,2]"
  )

  selected <- candidate_names[
    startsWith(candidate_names, "rescor__")
  ]

  testthat::expect_identical(
    selected,
    "rescor__pupilleft__pupilright"
  )
})

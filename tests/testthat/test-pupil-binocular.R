test_that("binocular preparation does not average eyes", {
  sim <- simulate_binocular_pupil_timecourse(seed = 51, n_participants = 4, trials_per_participant = 2, time_points = 12)
  prep <- prepare_binocular_pupil_timecourse(sim$data)
  expect_s3_class(prep, "gp3bayes_binocular_pupil_prepared")
  expect_true(all(c("pupil_left", "pupil_right") %in% names(prep$data)))
  expect_false("pupil" %in% names(prep$data))
  audit <- audit_binocular_pupil_readiness(prep)
  expect_s3_class(audit, "gp3bayes_binocular_pupil_audit")
})

test_that("binocular specification supports Gaussian and Student families", {
  sim <- simulate_binocular_pupil_timecourse(seed = 52, n_participants = 4, trials_per_participant = 2, time_points = 12)
  prep <- prepare_binocular_pupil_timecourse(sim$data)
  a <- specify_binocular_pupil_model(prep, family = "gaussian")
  b <- specify_binocular_pupil_model(prep, family = "student", residual_correlation = TRUE)
  expect_s3_class(a, "gp3bayes_binocular_pupil_specification")
  expect_identical(b$family, "student")
  expect_true(b$residual_correlation)
})


test_that("binocular preparation rejects a non-comparative one-condition design", {
  sim <- simulate_binocular_pupil_timecourse(seed = 53, n_participants = 4, trials_per_participant = 2, time_points = 12)
  one <- sim$data
  one$condition <- factor("control")
  expect_error(prepare_binocular_pupil_timecourse(one), "at least two observed condition levels")
  expect_error(
    specify_binocular_pupil_model(
      prepare_binocular_pupil_timecourse(sim$data),
      smooth_basis_dimension = 8.5
    ),
    "integer"
  )
})

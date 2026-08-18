test_that("response-shape simulation is deterministic", {
  a <- simulate_pupil_response_shape(seed = 61, n_participants = 4, trials_per_participant = 2, time_points = 12)
  b <- simulate_pupil_response_shape(seed = 61, n_participants = 4, trials_per_participant = 2, time_points = 12)
  expect_identical(a$data, b$data)
  expect_identical(a$truth, b$truth)
})

test_that("experimental response-shape specification remains narrow", {
  sim <- simulate_pupil_response_shape(seed = 62, n_participants = 4, trials_per_participant = 2, time_points = 12)
  spec <- specify_pupil_response_shape_model(sim$data)
  expect_s3_class(spec, "gp3bayes_pupil_response_shape_specification")
  expect_true(spec$experimental)
  expect_false(spec$fit_performed)
  expect_error(specify_pupil_response_shape_model(sim$data, condition_effects = "arbitrary"), "Unsupported")
})

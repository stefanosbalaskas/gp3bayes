test_that("advanced sensitivity suite materializes alternatives without fitting", {
  sim <- simulate_advanced_pupil_timecourse(seed = 41, n_participants = 4, trials_per_participant = 2, time_points = 12)
  spec <- specify_advanced_pupil_timecourse_model(sim$data, family = "gaussian", autocorrelation = "none")
  suite <- create_pupil_advanced_sensitivity_suite(spec)
  expect_s3_class(suite, "gp3bayes_pupil_advanced_sensitivity_suite")
  expect_true(nrow(suite$scenarios) > 3)
  alt_name <- suite$scenarios$scenario[suite$scenarios$dimension == "family"][[1L]]
  alt <- materialize_pupil_advanced_sensitivity_scenario(suite, alt_name)
  expect_s3_class(alt, "gp3bayes_pupil_advanced_specification")
  expect_identical(alt$family, "student")
  expect_false(alt$fit_performed)
})

test_that("model card records governance and target", {
  sim <- simulate_advanced_pupil_timecourse(seed = 42, n_participants = 4, trials_per_participant = 2, time_points = 12)
  spec <- specify_advanced_pupil_timecourse_model(sim$data, predictive_target = "future_segment")
  card <- pupil_model_card(spec)
  expect_s3_class(card, "gp3bayes_pupil_model_card")
  expect_true(any(card$table$field == "predictive_target" & card$table$value == "future_segment"))
  expect_true(length(card$governance) >= 3)
})

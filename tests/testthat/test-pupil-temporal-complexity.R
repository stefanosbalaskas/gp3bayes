test_that("temporal audit reports within-series dependence", {
  sim <- simulate_advanced_pupil_timecourse(seed = 31, n_participants = 5, trials_per_participant = 3, time_points = 20, ar = 0.6)
  audit <- audit_pupil_temporal_dependence(sim$data, max_lag = 5)
  expect_s3_class(audit, "gp3bayes_pupil_temporal_dependence_audit")
  expect_equal(nrow(audit$series), 15)
  expect_true("median_lag1" %in% audit$summary$metric)
})

test_that("complexity audit flags large exact GP before fitting", {
  sim <- simulate_advanced_pupil_timecourse(seed = 32, n_participants = 2, trials_per_participant = 1, time_points = 600)
  expect_error(
    specify_advanced_pupil_timecourse_model(
      sim$data,
      temporal_structure = "gaussian_process",
      gp_spec = create_pupil_gp_spec("matern32", "exact"),
      allow_high_complexity = FALSE
    ),
    "complexity budget"
  )
})

test_that("approximate GP remains within default budget for modest data", {
  sim <- simulate_advanced_pupil_timecourse(seed = 33, n_participants = 4, trials_per_participant = 2, time_points = 20)
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    temporal_structure = "gaussian_process",
    gp_spec = create_pupil_gp_spec("matern52", "approximate", 20)
  )
  expect_true(spec$complexity_audit$overall_status %in% c("ok", "review"))
})

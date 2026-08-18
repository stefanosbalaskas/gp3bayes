test_that("backend-free 0.5 graphics return their plotting data", {
  sim <- simulate_advanced_pupil_timecourse(seed = 71, n_participants = 4, trials_per_participant = 2, time_points = 12)
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  z1 <- plot_advanced_pupil_simulation(sim)
  audit <- audit_pupil_temporal_dependence(sim$data)
  z2 <- plot_pupil_temporal_dependence(audit)
  spec <- specify_advanced_pupil_timecourse_model(sim$data)
  z3 <- plot_pupil_model_complexity(spec)
  expect_true(is.data.frame(z1))
  expect_true(is.data.frame(z2))
  expect_true(is.data.frame(z3))
})

test_that("missingness and measurement audit plots are backend-free", {
  sim <- simulate_advanced_pupil_timecourse(seed = 72, n_participants = 4, trials_per_participant = 2, time_points = 12)
  mm <- create_pupil_measurement_model(baseline_error = "baseline_se")
  ms <- create_pupil_missingness_spec(response = "model")
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    covariates = "baseline_pupil",
    measurement_model = mm,
    missingness_model = ms
  )
  ma <- audit_pupil_measurement_model(spec)
  mia <- audit_pupil_missingness(spec)
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  expect_true(is.data.frame(plot_pupil_measurement_uncertainty(ma)))
  expect_true(is.data.frame(plot_pupil_missingness(mia)))
})

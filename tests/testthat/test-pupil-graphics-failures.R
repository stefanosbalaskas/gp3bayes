test_that("pupil graphics return ggplot objects when ggplot2 is available", {
  skip_if_not_installed("ggplot2")
  sim <- simulate_pupil_timecourse(
    n_participants=2,trials_per_participant=3,sampling_frequency=10,
    blink_trial_probability=0,seed=5
  )
  c <- create_pupil_contract(
    "pupil_mm","participant_id","trial_id","event_time",
    "millimetres",10,condition_col="condition",
    gaze_x_col="gaze_x",gaze_y_col="gaze_y",luminance_col="luminance"
  )
  p <- prepare_pupil_timecourse(sim$data,c)
  expect_s3_class(plot_pupil_readiness(p$audit),"ggplot")
  expect_s3_class(plot_pupil_observed_trajectory(p),"ggplot")
  expect_s3_class(plot_pupil_measurement_audit(audit_pupil_measurement_context(p)),"ggplot")

  grid <- expand.grid(.event_time=seq(0,1,length.out=5),
                      .condition=factor(c("control","treatment")))
  pred <- as_pupil_prediction_draws(matrix(rnorm(1000),100,nrow(grid)),
                                    grid,"millimetres")
  tr <- estimate_pupil_trajectory(pred)
  est <- estimate_pupil_window(pred,c(.2,.8))
  expect_s3_class(plot_pupil_posterior_trajectory(tr),"ggplot")
  expect_s3_class(plot_pupil_estimand(est),"ggplot")
})

test_that("failure contracts reject malformed and unsafe requests", {
  expect_error(create_pupil_validation_plan(list()),"prepared")
  expect_error(as_pupil_prediction_draws(matrix(NA_real_,2,2),
                                         data.frame(.event_time=1:2),
                                         "millimetres"),"finite")
  expect_error(
    simulate_pupil_timecourse(n_participants=1000,trials_per_participant=1000,
                              max_rows=1000),
    "exceeding"
  )
})

test_that("backend interfaces remain restricted", {
  expect_false("..." %in% names(formals(fit_pupil_model_backend)))
  expect_false("formula" %in% names(formals(fit_pupil_model_backend)))
  expect_false("family" %in% names(formals(fit_pupil_model_backend)))
  expect_false("..." %in% names(formals(specify_pupil_timecourse_model)))
})


test_that("pupil PPC feature helpers retain peak, latency, AUC and window evidence", {
  d <- data.frame(
    .pupil_model = c(1, 2, 3, 2, 1, 2, 3, 2),
    .event_time = rep(c(0, 1, 2, 3), 2),
    .series_id = factor(rep(c("a", "b"), each = 4)),
    .participant = factor(rep(c("p1", "p2"), each = 4)),
    .trial = factor(rep(c("t1", "t2"), each = 4))
  )
  yrep <- rbind(d$.pupil_model, d$.pupil_model + 0.2, d$.pupil_model - 0.1)
  features <- .gp3p_ppc_features(yrep, d, window = c(1, 3))
  expect_true(all(c(
    "peak_response", "peak_latency", "auc", "declared_window_mean"
  ) %in% features$statistic))
  heterogeneity <- .gp3p_group_heterogeneity(
    yrep, d$.pupil_model, d$.participant, "participant"
  )
  expect_equal(heterogeneity$n_groups, 2L)
})

test_that("malformed fitted pupil objects are rejected by diagnostic data access", {
  malformed <- structure(
    list(fit_performed = TRUE, translation = list(data = NULL)),
    class = "gp3bayes_pupil_fit"
  )
  expect_error(.gp3p_fit_data(malformed), "Malformed")
})

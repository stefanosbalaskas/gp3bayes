test_that("predictive atlas table accessor preserves draw statistics", {
  x <- structure(
    list(
      draw_statistics = data.frame(
        draw = 1:2,
        mean = c(1, 2),
        sd = c(1, 1),
        median = c(1, 2),
        q10 = c(0, 1),
        q90 = c(2, 3)
      )
    ),
    class = "gp3bayes_predictive_distribution_atlas"
  )
  expect_equal(nrow(predictive_distribution_atlas_table(x)), 2L)
})

test_that("predictive quantile and score plots return ggplots", {
  skip_if_not_installed("ggplot2")

  q <- data.frame(
    probability = c(0.1, 0.5, 0.9),
    observed_quantile = c(1, 2, 3),
    predictive_lower = c(0.8, 1.8, 2.8),
    predictive_median = c(1.1, 2.1, 3.1),
    predictive_upper = c(1.4, 2.4, 3.4)
  )
  expect_s3_class(plot_predictive_quantile_envelope(q), "ggplot")

  score <- structure(
    list(
      family = "binary",
      scope = "fitted_prepared_data",
      draws = data.frame(
        draw = rep(1:20, 2),
        metric = rep(c("brier", "log_loss"), each = 20),
        value = runif(40)
      ),
      summary = data.frame(
        metric = c("brier", "log_loss"),
        mean = c(0.2, 0.5),
        lower = c(0.1, 0.4),
        median = c(0.2, 0.5),
        upper = c(0.3, 0.6)
      )
    ),
    class = "gp3bayes_prediction_score_uncertainty"
  )

  expect_equal(nrow(prediction_score_uncertainty_table(score)), 2L)
  expect_s3_class(plot_prediction_score_uncertainty(score), "ggplot")
})

test_that("calibration-uncertainty plot adapter is descriptive", {
  skip_if_not_installed("ggplot2")

  cal <- data.frame(
    observed_rate = c(0.1, 0.5, 0.9),
    predicted_lower = c(0.05, 0.4, 0.8),
    predicted_median = c(0.1, 0.5, 0.9),
    predicted_upper = c(0.15, 0.6, 0.95)
  )
  expect_s3_class(plot_binary_calibration_uncertainty(cal), "ggplot")
})

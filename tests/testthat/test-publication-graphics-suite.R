test_that("publication theme returns a ggplot theme", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(theme_gp3bayes(), "theme")
})

test_that("posterior interval graphics accept numeric draws", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("bayesplot")
  draws <- cbind(
    alpha = seq(-1, 1, length.out = 100),
    beta = seq(0, 2, length.out = 100)
  )
  expect_s3_class(plot_posterior_intervals(draws), "ggplot")
  expect_s3_class(plot_posterior_areas(draws), "ggplot")
  expect_s3_class(plot_posterior_correlations(draws), "ggplot")
})

test_that("calibration and coverage plots return ggplots", {
  skip_if_not_installed("ggplot2")
  calibration <- data.frame(
    mean_predicted_probability = c(0.2, 0.5, 0.8),
    observed_rate = c(0.25, 0.45, 0.75),
    posterior_lower = c(0.15, 0.35, 0.65),
    posterior_upper = c(0.30, 0.55, 0.85)
  )
  coverage <- data.frame(
    nominal_coverage = c(0.5, 0.8, 0.95),
    empirical_coverage = c(0.52, 0.79, 0.93),
    mean_interval_width = c(1, 2, 3)
  )
  expect_s3_class(plot_binary_calibration(calibration), "ggplot")
  expect_s3_class(plot_predictive_coverage(coverage), "ggplot")
})

test_that("figure-set constructor is governed", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3),
    ggplot2::aes(x, y)
  ) + ggplot2::geom_point()
  fs <- create_figure_set(example = p)
  expect_s3_class(fs, "gp3bayes_figure_set")
  expect_equal(fs$names, "example")
})

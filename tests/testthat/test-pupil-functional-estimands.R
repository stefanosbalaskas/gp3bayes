test_that("functional trajectory estimands are deterministic and governed", {
  set.seed(1)
  grid <- expand.grid(
    time = seq(0, 1000, length.out = 21),
    condition = factor(c("A", "B"), levels = c("A", "B")),
    KEEP.OUT.ATTRS = FALSE
  )
  grid <- grid[order(grid$condition, grid$time), ]
  mu <- ifelse(grid$condition == "A", sin(grid$time / 300), 0.5 * sin(grid$time / 300))
  draws <- matrix(rnorm(500 * nrow(grid), rep(mu, each = 500), 0.05), nrow = 500)
  spec <- list(mapping = list(time = "time", condition = "condition"))
  pred <- structure(
    list(grid = grid, draws = draws, specification = spec),
    class = c("gp3bayes_pupil_advanced_trajectory", "gp3bayes_pupil_trajectory")
  )

  d1 <- estimate_pupil_trajectory_derivative(pred, order = 1)
  expect_s3_class(d1, "gp3bayes_pupil_trajectory_derivative")
  expect_equal(nrow(d1$grid), 40L)
  expect_equal(ncol(d1$draws), 40L)
  expect_equal(unique(pupil_trajectory_derivative_table(d1)$derivative_order), 1L)

  con <- estimate_pupil_dynamic_contrast(pred, c("A", "B"), threshold = 0.1)
  expect_s3_class(con, "gp3bayes_pupil_dynamic_contrast")
  expect_equal(nrow(con$table), 21L)
  expect_true(all(con$table$probability_above_threshold >= 0 & con$table$probability_above_threshold <= 1))

  dur <- estimate_pupil_threshold_duration(con, direction = "absolute", threshold = 0.1)
  expect_s3_class(dur, "gp3bayes_pupil_threshold_duration")
  expect_true(dur$summary$mean >= 0)
})

test_that("variance-partition table adapter is strict", {
  x <- structure(
    list(
      table = data.frame(
        component = c("participant", "residual"),
        fraction_median = c(0.2, 0.8),
        fraction_lower = c(0.1, 0.7),
        fraction_upper = c(0.3, 0.9)
      )
    ),
    class = "gp3bayes_random_intercept_variance_partition"
  )
  expect_equal(nrow(random_intercept_variance_partition_table(x)), 2L)
})

test_that("hierarchical publication plots return ggplots", {
  skip_if_not_installed("ggplot2")

  draws <- data.frame(
    group = "participant",
    level = rep(c("A", "B"), each = 100),
    coefficient = "Intercept",
    draw = rep(1:100, times = 2),
    value = c(rnorm(100, -0.5, 0.1), rnorm(100, 0.5, 0.1))
  )
  ranks <- data.frame(
    level = c("A", "B"),
    mean_rank = c(2, 1),
    probability_highest = c(0, 1)
  )
  partition <- data.frame(
    component = c("participant", "residual"),
    fraction_median = c(0.2, 0.8),
    fraction_lower = c(0.1, 0.7),
    fraction_upper = c(0.3, 0.9)
  )

  expect_s3_class(plot_group_effect_distribution(draws), "ggplot")
  expect_s3_class(plot_group_effect_rank_probability(ranks), "ggplot")
  expect_s3_class(plot_random_intercept_variance_partition(partition), "ggplot")
})

test_that("LOO influence aggregates by a declared group", {
  d <- data.frame(
    pareto_k = c(0.1, 0.8, 0.2, 1.1),
    elpd_loo = c(-1, -2, -1.5, -3),
    participant = c("A", "A", "B", "B")
  )

  out <- loo_group_influence_table(d, "participant")
  expect_equal(nrow(out), 2L)
  expect_false(any(out$automatic_group_exclusion))
  expect_equal(out$flagged_k_ge_0_7[out$group_value == "A"], 1L)
})

test_that("grouped LOO plots return ggplots", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(
    group_value = c("A", "B"),
    max_pareto_k = c(0.8, 1.1),
    total_elpd_loo = c(-3, -4.5)
  )
  expect_s3_class(plot_loo_group_influence(d), "ggplot")
  expect_s3_class(plot_loo_group_elpd(d), "ggplot")
})

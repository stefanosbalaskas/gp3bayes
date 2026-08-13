test_that("LOO atlas uses pointwise predictive evidence", {
  skip_if_not_installed("loo")
  set.seed(1)
  log_lik <- matrix(rnorm(500 * 12, -1, 0.15), nrow = 500, ncol = 12)
  x <- compute_psis_loo_from_log_lik(log_lik, cores = 1, save_psis = TRUE)
  d <- loo_pointwise_table(x)
  expect_equal(nrow(d), 12L)
  expect_true(all(c("pareto_k", "elpd_loo") %in% names(d)))
  atlas <- create_loo_influence_atlas(x)
  expect_s3_class(atlas, "gp3bayes_loo_influence_atlas")
  expect_false(atlas$automatic_exclusion)
})

test_that("LOO atlas graphics return ggplots", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(
    observation = 1:4,
    elpd_loo = c(-1, -1.2, -0.8, -2),
    pareto_k = c(0.1, 0.2, 0.8, 1.1)
  )
  expect_s3_class(plot_loo_pointwise_elpd(d), "ggplot")
  expect_s3_class(plot_loo_pareto_vs_elpd(d), "ggplot")
  expect_s3_class(plot_loo_influence_rank(d), "ggplot")
})

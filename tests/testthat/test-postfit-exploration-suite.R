test_that("posterior tables work with deterministic numeric draws", {
  draws <- cbind(
    alpha = seq(-1, 1, length.out = 200),
    beta = seq(0, 2, length.out = 200),
    gamma = seq(1, -1, length.out = 200)
  )

  intervals <- posterior_interval_table(draws)
  expect_equal(nrow(intervals), 3L)
  expect_true(all(c("lower", "median", "upper") %in% names(intervals)))

  probs <- posterior_probability_table(draws, rope = c(-0.1, 0.1))
  expect_equal(nrow(probs), 3L)
  expect_true(all(probs$probability_gt_zero >= 0))
  expect_true(all(probs$probability_gt_zero <= 1))

  corrs <- posterior_correlation_table(draws)
  expect_equal(nrow(corrs), 3L)
  expect_true(all(is.finite(corrs$correlation)))
})

test_that("MCMC issue table is explicit and conservative", {
  d <- data.frame(
    variable = c("a", "b"),
    sd = c(1, 1),
    rhat = c(1, 1.03),
    ess_bulk = c(1000, 100),
    ess_tail = c(900, 120),
    mcse_mean = c(0.01, 0.2)
  )
  out <- identify_mcmc_issues(d)
  expect_false(out$flagged[[1L]])
  expect_true(out$flagged[[2L]])
})

test_that("LOO weight table never selects automatically", {
  x <- c(model_a = 0.7, model_b = 0.3)
  out <- model_weights_table(x)
  expect_equal(out$weight, unname(x))
  expect_false(any(out$automatic_selection))
})

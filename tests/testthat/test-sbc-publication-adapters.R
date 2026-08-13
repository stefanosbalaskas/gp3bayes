test_that("SBC table adapters preserve conservative metadata", {
  x <- structure(
    list(
      status = "review",
      plan = list(family = "binary", backend = "example", n_sims = 50L),
      raw = list(
        stats = data.frame(
          variable = c("a", "b"),
          rank = c(1, 2),
          max_rank = c(100, 100)
        )
      ),
      diagnostics_inspected = TRUE,
      calibration_established = FALSE
    ),
    class = "gp3bayes_sbc_result"
  )
  expect_equal(nrow(sbc_stats_table(x)), 2L)
  overview <- sbc_overview_table(x)
  expect_equal(overview$simulations, 50L)
  expect_false(overview$calibration_established)
})

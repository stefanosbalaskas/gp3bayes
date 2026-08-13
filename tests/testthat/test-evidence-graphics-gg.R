test_that("table adapters preserve existing governance objects", {
  sensitivity <- structure(
    list(
      component_status = data.frame(
        component = c("prior_scale", "psis_loo"),
        status = c("completed", "pass"),
        detail = c("done", "done")
      )
    ),
    class = "gp3bayes_sensitivity_suite"
  )
  expect_equal(nrow(sensitivity_suite_table(sensitivity)), 2L)

  manifest <- structure(
    list(
      table = data.frame(
        component = c("family", "seed"),
        identical = c(TRUE, FALSE),
        left = c("binary", "1"),
        right = c("binary", "2")
      )
    ),
    class = "gp3bayes_manifest_comparison"
  )
  expect_equal(nrow(manifest_comparison_table(manifest)), 2L)
})

test_that("gg evidence adapters return plots", {
  skip_if_not_installed("ggplot2")

  backend <- structure(
    list(
      table = data.frame(
        variable = c("b_a", "b_b"),
        rstan_mean = c(0.1, 0.2),
        cmdstanr_mean = c(0.11, 0.19),
        status = c("pass", "pass")
      )
    ),
    class = "gp3bayes_backend_parity_audit"
  )

  expect_s3_class(plot_backend_parity_gg(backend), "ggplot")
})

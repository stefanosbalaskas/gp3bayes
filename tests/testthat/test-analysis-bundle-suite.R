test_that("analysis bundle accessors preserve component status", {
  obj <- structure(
    list(
      family = "binary",
      status = data.frame(
        component = c("posterior", "mcmc"),
        available = c(TRUE, FALSE),
        error = c("", "not available")
      ),
      components = list(
        posterior = list(ok = TRUE, value = data.frame(a = 1), error = NULL),
        mcmc = list(ok = FALSE, value = NULL, error = "not available")
      ),
      automatic_decision = FALSE,
      interpretation = "No automatic decision."
    ),
    class = "gp3bayes_analysis_bundle"
  )
  expect_equal(analysis_bundle_table(obj), obj$status)
  tables <- create_publication_table_set(obj)
  expect_true("posterior" %in% names(tables))
  expect_false("mcmc" %in% names(tables))
})

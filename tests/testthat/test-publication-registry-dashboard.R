test_that("publication registry is explicit and valid", {
  r <- create_publication_registry("example")
  r <- register_publication_table(
    r,
    "posterior",
    data.frame(variable = "b_x", median = 0.2),
    caption = "Posterior summary",
    source = "example"
  )
  expect_equal(nrow(publication_registry_table(r)), 1L)
  expect_true(validate_publication_registry(r)$valid)
  expect_false(validate_publication_registry(r)$automatic_writing)
})

test_that("registry accepts ggplot figures", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3),
    ggplot2::aes(x, y)
  ) + ggplot2::geom_point()
  r <- register_publication_figure(create_publication_registry(), "figure1", p)
  expect_equal(length(r$figures), 1L)
})

test_that("evidence inventory remains descriptive", {
  inv <- create_complete_evidence_inventory(
    diagnostic = structure(list(status = "review"), class = "example"),
    table = data.frame(x = 1)
  )
  expect_equal(nrow(evidence_inventory_table(inv)), 2L)
  expect_false(any(inv$table$automatic_decision))
})

test_that("dashboard indexes supplied evidence without running analyses", {
  card <- structure(list(), class = "gp3bayes_model_card")
  d <- create_diagnostic_dashboard(model_card = card, label = "example")
  expect_s3_class(d, "gp3bayes_diagnostic_dashboard")
  expect_true(
    diagnostic_dashboard_table(d)$available[
      diagnostic_dashboard_table(d)$component == "model_card"
    ]
  )
  expect_false(d$automatic_decision)
})

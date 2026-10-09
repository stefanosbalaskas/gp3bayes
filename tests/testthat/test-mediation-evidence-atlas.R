test_that("mediation evidence atlas plots use declared inputs", {
  skip_if_not_installed("ggplot2")
  effects <- data.frame(
    term = c("via surveillance", "via control"),
    estimate = c(0.18, -0.12), lower = c(0.05, -0.25),
    upper = c(0.30, 0.01)
  )
  p <- plot_mediation_evidence_atlas(effects)
  expect_s3_class(p, "ggplot")
  expect_error(plot_mediation_evidence_atlas(transform(
    effects, lower = estimate + 1
  )), "within")
  b <- data.frame(term = rep("indirect", 30L), draw = seq(-0.5, 0.5, length.out = 30))
  expect_s3_class(plot_mediation_evidence_atlas(b, "bootstrap"), "ggplot")
})

test_that("graph disagreement is structurally descriptive", {
  skip_if_not_installed("ggplot2")
  theory <- data.frame(from = c("X", "M"), to = c("M", "Y"))
  discovered <- data.frame(from = "X", to = "M")
  p <- plot_mediation_evidence_atlas(
    type = "dag", theory_edges = theory, discovered_edges = discovered
  )
  expect_s3_class(p, "ggplot")
  expect_setequal(as.character(attr(p, "edge_audit")$status), c("Both", "Theory only"))
  expect_error(plot_mediation_evidence_atlas(
    type = "dag", theory_edges = theory, discovered_edges = data.frame()
  ), "require")
})

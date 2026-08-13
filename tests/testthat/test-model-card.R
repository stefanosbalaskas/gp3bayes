test_that("reporting checklist accepts a model card", {
  card <- structure(
    list(
      label = "example",
      family = "binary",
      model_family = "bernoulli_logit",
      formula = "y ~ x + (1 | id)",
      sampling_backend = "rstan",
      sampling = list(chains = 2),
      diagnosis = list(status = "pass"),
      workflow = list(status = "completed"),
      analysis_bundle = NULL,
      manifest = NULL,
      evidence = data.frame(
        component = "model_fit",
        status = "available"
      ),
      interpretation = "Documentation only."
    ),
    class = "gp3bayes_model_card"
  )

  out <- create_reporting_checklist(card)
  expect_true(is.data.frame(out))
  expect_false(any(out$automatic_requirement))
})

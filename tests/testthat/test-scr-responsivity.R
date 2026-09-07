test_that("Bayesian SCR responsivity retains low-reactive participants", {
  out <- gp3bayes:::estimate_scr_responsivity_bayes(
    c("p1", "p1", "p2", "p2"), c(.03, .04, 0, 0)
  )
  p2 <- out[out$participant == "p2", ]
  expect_true(p2$conventional_nonresponder)
  expect_true(p2$retain_for_modeling)
  expect_true(p2$posterior_response_probability > 0)
  expect_true(p2$posterior_response_probability < 1)
})

test_that("posterior response probability increases with responses", {
  out <- gp3bayes:::estimate_scr_responsivity_bayes(
    rep(c("low", "high"), each = 4), c(0, 0, .01, 0, .03, .04, .05, .06)
  )
  expect_gt(
    out$posterior_response_probability[out$participant == "high"],
    out$posterior_response_probability[out$participant == "low"]
  )
})

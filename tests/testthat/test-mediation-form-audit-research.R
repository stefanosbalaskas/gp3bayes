test_that("model-form audit is participant-clustered and reproducible", {
  set.seed(2026)
  n <- 144L
  d <- data.frame(
    participant_id = rep(seq_len(24), each = 6L),
    x = rep(c(0, 1), length.out = n)
  )
  d$m <- .5 + .8 * d$x + rnorm(n, sd = .5)
  d$y <- .2 + .3 * d$x + .5 * d$m + .4 * d$m^2 + rnorm(n, sd = .4)
  before <- .Random.seed
  a <- audit_mediation_functional_form(
    d, "x", "m", "y", resamples = 25, seed = 101
  )
  expect_identical(.Random.seed, before)
  b <- audit_mediation_functional_form(
    d, "x", "m", "y", resamples = 25, seed = 101
  )
  expect_equal(a$bootstrap_ledger, b$bootstrap_ledger)
  expect_equal(a$n_participants, 24L)
  expect_setequal(a$estimates$model, c("linear", "quadratic"))
  expect_true(all(is.finite(a$estimates$mediator_shift_estimate)))
  expect_match(a$claim_boundary, "no causal identification")
  d$x[1L] <- 2
  expect_error(audit_mediation_functional_form(
    d, "x", "m", "y", resamples = 25
  ), "0/1")
})

make_gp3b_mediation_prepared <- function(balanced_between = FALSE) {
  participant_id <- rep(sprintf("p%02d", 1:8), each = 6)
  trial_id <- rep(1:6, 8)
  x <- rep(c(0, 1, 0, 1, 0, 1), 8)
  if (!balanced_between) x[c(6, 12, 19, 25)] <- c(0, 0, 1, 1)
  dwell <- 1 + 0.5 * x + rep(seq(-0.2, 0.2, length.out = 8), each = 6) + rep(c(-.1, .1, 0, .05, -.05, .1), 8)
  outcome <- as.integer((seq_along(x) + x) %% 3 == 0)
  x_between <- ave(x, participant_id, FUN = mean)
  m_between <- ave(dwell, participant_id, FUN = mean)
  data <- data.frame(
    participant_id = participant_id,
    trial_id = trial_id,
    x = x,
    dwell = dwell,
    outcome = outcome,
    X_within = x - x_between,
    X_between = x_between,
    M_within = dwell - m_between,
    M_between = m_between,
    mediation_analysis_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      data = data,
      columns = list(
        participant = "participant_id", trial = "trial_id", x = "x",
        mediator = "dwell", outcome = "outcome", quality = NULL
      ),
      provenance = list(source_id = "synthetic-test")
    ),
    class = "eye_multilevel_mediation_data"
  )
}

test_that("simple mediation specification preserves explicit contract", {
  spec <- specify_multilevel_gaze_mediation(
    make_gp3b_mediation_prepared(),
    mediator_family = "normal",
    outcome_family = "binary",
    random_slopes = character()
  )
  expect_s3_class(spec, "gp3bayes_multilevel_mediation_specification")
  expect_identical(spec$mediator_family, "gaussian")
  expect_identical(spec$outcome_family, "bernoulli")
  expect_true(all(c("a_within", "b_within") %in% spec$estimable_paths))
  expect_identical(spec$provenance$model_specification$estimand_scale, "linear_predictor_product")
})

test_that("balanced within-subject X does not manufacture between X paths", {
  spec <- specify_multilevel_gaze_mediation(
    make_gp3b_mediation_prepared(TRUE), random_slopes = character()
  )
  expect_false("a_between" %in% spec$estimable_paths)
  expect_false("cprime_between" %in% spec$estimable_paths)
  expect_true("a_within" %in% spec$estimable_paths)
})

test_that("missingness requires explicit policy", {
  prepared <- make_gp3b_mediation_prepared()
  prepared$data$dwell[[3L]] <- NA_real_
  prepared$data$M_within[[3L]] <- NA_real_
  prepared$data$mediation_analysis_eligible[[3L]] <- FALSE
  expect_error(specify_multilevel_gaze_mediation(prepared), "explicit.*missingness_policy")
  spec <- specify_multilevel_gaze_mediation(
    prepared, random_slopes = character(), missingness_policy = "complete_case"
  )
  expect_equal(spec$analysis_rows, nrow(prepared$data) - 1L)
  expect_equal(spec$excluded_row_positions, 3L)
})

test_that("families and priors are validated before fitting", {
  prior <- create_mediation_prior_specification(coefficient_sd = 0.5)
  expect_equal(prior$coefficient_sd, 0.5)
  expect_error(create_mediation_prior_specification(coefficient_sd = 0), "positive")
  prepared <- make_gp3b_mediation_prepared()
  prepared$data$dwell[[1L]] <- 0
  expect_error(
    specify_multilevel_gaze_mediation(
      prepared, mediator_family = "lognormal", random_slopes = character()
    ),
    "strictly positive"
  )
})

test_that("sampling controls fail before backend execution", {
  expect_error(
    .gp3b_med_validate_sampling(2, 100, 100, 1, 1, .95, 10),
    "warmup.*smaller"
  )
  expect_error(
    .gp3b_med_validate_sampling(2, 200, 100, 3, 1, .95, 10),
    "cores.*cannot exceed"
  )
  expect_silent(.gp3b_med_validate_sampling(4, 200, 100, 2, 7, .9, 10))
})

test_that("serial preparation semantics are required", {
  expect_error(
    specify_multilevel_serial_gaze_mediation(make_gp3b_mediation_prepared()),
    "mediator2 semantics"
  )
})

test_that("moderated preparation semantics are required", {
  expect_error(
    specify_multilevel_moderated_gaze_mediation(make_gp3b_mediation_prepared()),
    "moderator_within"
  )
})

test_that("serial and moderated random slopes are rejected explicitly", {
  prepared <- make_gp3b_mediation_prepared()
  prepared$data$trust <- 2 + 0.3 * prepared$data$x + 0.2 * prepared$data$dwell
  trust_mean <- ave(prepared$data$trust, prepared$data$participant_id, FUN = mean)
  prepared$data$M2_within <- prepared$data$trust - trust_mean
  prepared$data$M2_between <- trust_mean
  prepared$columns$mediator2 <- "trust"
  prepared$columns$mediator2_within <- "M2_within"
  prepared$columns$mediator2_between <- "M2_between"
  expect_error(
    specify_multilevel_serial_gaze_mediation(prepared, random_slopes = "mediator_x"),
    "not yet implemented"
  )

  prepared$data$z <- rep(c(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5), 8)
  z_mean <- ave(prepared$data$z, prepared$data$participant_id, FUN = mean)
  prepared$data$Z_within <- prepared$data$z - z_mean
  prepared$data$Z_between <- z_mean
  prepared$columns$moderator <- "z"
  prepared$columns$moderator_within <- "Z_within"
  prepared$columns$moderator_between <- "Z_between"
  expect_error(
    specify_multilevel_moderated_gaze_mediation(prepared, random_slopes = "outcome_m"),
    "not yet implemented"
  )
})

test_that("serial between paths follow observed design support", {
  prepared <- make_gp3b_mediation_prepared(TRUE)
  prepared$data$trust <- 2 + 0.3 * prepared$data$x + 0.2 * prepared$data$dwell
  trust_mean <- ave(prepared$data$trust, prepared$data$participant_id, FUN = mean)
  prepared$data$M2_within <- prepared$data$trust - trust_mean
  prepared$data$M2_between <- trust_mean
  prepared$columns$mediator2 <- "trust"
  prepared$columns$mediator2_within <- "M2_within"
  prepared$columns$mediator2_between <- "M2_between"
  spec <- specify_multilevel_serial_gaze_mediation(prepared)
  expect_true(all(c("a1_within", "d_within", "b2_within") %in% spec$estimable_paths))
  expect_false("a1_between" %in% spec$estimable_paths)
})

test_that("moderated path requires moderator variation", {
  prepared <- make_gp3b_mediation_prepared()
  prepared$data$z <- 0
  prepared$data$Z_within <- 0
  prepared$data$Z_between <- 0
  prepared$columns$moderator <- "z"
  prepared$columns$moderator_within <- "Z_within"
  prepared$columns$moderator_between <- "Z_between"
  expect_error(
    specify_multilevel_moderated_gaze_mediation(prepared),
    "no observed variation"
  )
})

test_that("participant mediation plot refuses incomplete random-slope models", {
  spec <- specify_multilevel_gaze_mediation(
    make_gp3b_mediation_prepared(),
    random_slopes = "mediator_x"
  )
  fake_fit <- structure(
    list(specification = spec),
    class = "gp3bayes_multilevel_mediation_fit"
  )
  expect_error(
    plot_participant_mediation_effects(fake_fit),
    "did not estimate both participant-specific a and b paths"
  )
})


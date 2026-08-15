make_pupil_prepared <- function() {
  sim <- simulate_pupil_timecourse(
    n_participants = 4, trials_per_participant = 4,
    n_items = 4, sampling_frequency = 20,
    blink_trial_probability = 0, seed = 88
  )
  contract <- create_pupil_contract(
    "pupil_mm","participant_id","trial_id","event_time",
    "millimetres",20,item_col="item_id",condition_col="condition"
  )
  prepare_pupil_timecourse(sim$data, contract)
}

test_that("pupil specification is closed and inspectable", {
  p <- make_pupil_prepared()
  s <- specify_pupil_timecourse_model(
    p, smooth_basis_dimension = 6, autocorrelation = "ar1"
  )
  expect_s3_class(s, "gp3bayes_pupil_model_specification")
  expect_false(s$unrestricted_formula)
  expect_false(s$unrestricted_family)
  expect_match(s$formula_text, "s\\(\\.event_time")
  expect_match(s$formula_text, "ar\\(")
  expect_true(all(c("Intercept","b","sd","sigma","sds","ar") %in% s$priors$class))
})

test_that("ar1 is blocked for strongly irregular sampling", {
  p <- make_pupil_prepared()
  p$timing$cv_dt <- 1
  expect_error(specify_pupil_timecourse_model(p, autocorrelation = "ar1"),
               "sampling intervals are too irregular")
  expect_s3_class(
    specify_pupil_timecourse_model(p, autocorrelation = "none"),
    "gp3bayes_pupil_model_specification"
  )
})

test_that("arbitrary units require declared prior scales", {
  sim <- simulate_pupil_timecourse(
    n_participants = 2, trials_per_participant = 3,
    sampling_frequency = 10, seed = 4
  )
  d <- transform(sim$data, pupil_units = pupil_mm * 100)
  c <- create_pupil_contract(
    "pupil_units","participant_id","trial_id","event_time",
    "arbitrary_units",10,condition_col="condition"
  )
  p <- prepare_pupil_timecourse(d,c)
  expect_error(specify_pupil_timecourse_model(p, autocorrelation="none"),
               "prior_scales")
  s <- specify_pupil_timecourse_model(
    p, autocorrelation="none",
    prior_scales=c(intercept=100,coefficient=50,group_sd=30,residual=30,smooth_sd=30)
  )
  expect_s3_class(s, "gp3bayes_pupil_model_specification")
})

test_that("frozen prediction draws support trajectory and declared estimands", {
  grid <- expand.grid(
    .event_time = seq(0,1,length.out=11),
    .condition = factor(c("control","treatment")),
    KEEP.OUT.ATTRS = FALSE
  )
  set.seed(1)
  mu <- ifelse(grid$.condition=="treatment", .2, 0) + .3*grid$.event_time
  draws <- matrix(rnorm(400*nrow(grid), rep(mu, each=400), .05), nrow=400)
  pred <- as_pupil_prediction_draws(draws, grid, "millimetres")
  tr <- estimate_pupil_trajectory(pred)
  expect_s3_class(tr, "gp3bayes_pupil_trajectory")
  expect_equal(nrow(pupil_trajectory_table(tr)), nrow(grid))

  con <- pupil_condition_contrast(pred,c("treatment","control"),threshold=.1)
  expect_true(all(con$table$probability_gt_threshold >= 0 &
                  con$table$probability_gt_threshold <= 1))
  expect_s3_class(estimate_pupil_window(pred,c(.2,.8)), "gp3bayes_pupil_estimand")
  expect_s3_class(estimate_pupil_auc(pred,c(.2,.8)), "gp3bayes_pupil_estimand")
  expect_s3_class(estimate_pupil_peak(pred,c(.2,.8)), "gp3bayes_pupil_estimand")
  expect_s3_class(estimate_pupil_peak_latency(pred,c(.2,.8)), "gp3bayes_pupil_estimand")
})

test_that("prediction memory guards reject excessive arrays", {
  grid <- data.frame(.event_time = 1:10)
  expect_error(
    as_pupil_prediction_draws(matrix(0,100,10),grid,"millimetres",max_cells=100),
    "exceeds"
  )
})


test_that("hierarchy and single-level optional factors have explicit edge contracts", {
  sim1 <- simulate_pupil_timecourse(
    n_participants = 1, trials_per_participant = 3,
    n_items = NULL, conditions = "only", sampling_frequency = 10,
    blink_trial_probability = 0, seed = 9
  )
  c1 <- create_pupil_contract(
    "pupil_mm", "participant_id", "trial_id", "event_time",
    "millimetres", 10, condition_col = "condition"
  )
  p1 <- prepare_pupil_timecourse(sim1$data, c1)
  expect_error(
    specify_pupil_timecourse_model(p1, autocorrelation = "none"),
    "at least two participants"
  )

  sim2 <- simulate_pupil_timecourse(
    n_participants = 3, trials_per_participant = 3,
    n_items = NULL, conditions = "only", sampling_frequency = 10,
    blink_trial_probability = 0, seed = 10
  )
  p2 <- prepare_pupil_timecourse(sim2$data, c1)
  s2 <- specify_pupil_timecourse_model(p2, autocorrelation = "none")
  expect_false(s2$condition_trajectory)
  expect_false(s2$item_effects)
  expect_false(grepl(".condition", s2$formula_text, fixed = TRUE))
})


test_that("pupil prior predictive checking is governed and inert by default", {
  p <- make_pupil_prepared()
  s <- specify_pupil_timecourse_model(
    p, smooth_basis_dimension = 6, autocorrelation = "none"
  )
  pp <- check_pupil_prior_predictive(
    s, execute = FALSE, draws = 50L, chains = 2L, iter = 100L, warmup = 50L
  )
  expect_s3_class(pp, "gp3bayes_pupil_prior_predictive")
  expect_false(pp$executed)
  expect_false(pp$priors_changed)
  expect_false(pp$adequacy_certified)
  expect_true(all(c("family", "backend", "draws", "execute") %in%
                    as.data.frame(pp)$field))
})

test_that("backend-portable pupil wrappers have identical schemas", {
  p <- make_pupil_prepared()
  s <- specify_pupil_timecourse_model(
    p, smooth_basis_dimension = 6, autocorrelation = "none"
  )
  translation <- list(data = p$data[!is.na(p$data$.pupil_model), , drop = FALSE])
  sampling <- list(
    chains = 2L, iter = 100L, warmup = 50L, cores = 1L,
    adapt_delta = 0.95, max_treedepth = 12L, refresh = 0L, seed = 1L
  )
  a <- .gp3p_new_pupil_fit(
    s, translation, backend_fit = structure(list(), class = "dummy"),
    backend = "rstan", sampling = sampling,
    package_versions = c(brms = "test", backend = "test")
  )
  b <- .gp3p_new_pupil_fit(
    s, translation, backend_fit = structure(list(), class = "dummy"),
    backend = "cmdstanr", sampling = sampling,
    package_versions = c(brms = "test", backend = "test")
  )
  expect_identical(names(a), names(b))
  expect_identical(class(a), class(b))
  expect_identical(a$unrestricted_formula, FALSE)
  expect_identical(b$unrestricted_family, FALSE)
})

test_that("participant-conditioned prediction cannot silently choose a participant", {
  dummy <- structure(
    list(
      fit_performed = TRUE,
      specification = list(prepared = make_pupil_prepared(), covariates = character()),
      outcome_unit = "millimetres"
    ),
    class = "gp3bayes_pupil_fit"
  )
  expect_error(
    predict_pupil_trajectory(dummy, population_only = FALSE),
    "explicit `newdata`"
  )
})

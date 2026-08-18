# Experimental interpretable nonlinear pupil response-shape models for 0.5.

#' Specify the experimental nonlinear pupil response-shape model
#'
#' Uses a smooth asymmetric gated response:
#' baseline + exp(log_amplitude) * logistic((time-onset)/exp(log_rise)) *
#' logistic((onset+exp(log_duration)-time)/exp(log_decay)).
#'
#' Condition effects can enter log-amplitude, onset, and log-duration. This is
#' a deliberately single, inspectable response family rather than an arbitrary
#' nonlinear-formula interface.
#'
#' @param prepared A prepared pupil object or compatible data frame.
#' @param family Gaussian or Student-t.
#' @param condition_effects Character subset of `"amplitude"`, `"onset"`,
#'   and `"duration"`.
#' @param participant_effects Character subset of `"baseline"` and
#'   `"amplitude"`.
#' @param covariates Additional covariates for baseline only.
#' @param prior_scales Optional named positive numeric prior-scale overrides.
#' @return A `gp3bayes_pupil_response_shape_specification` object.
#' @export
specify_pupil_response_shape_model <- function(
    prepared,
    family = c("gaussian", "student"),
    condition_effects = c("amplitude", "onset", "duration"),
    participant_effects = c("baseline", "amplitude"),
    covariates = character(),
    prior_scales = NULL) {

  family <- match.arg(family)
  allowed_condition <- c("amplitude", "onset", "duration")
  allowed_participant <- c("baseline", "amplitude")
  if (!is.character(condition_effects) || any(!condition_effects %in% allowed_condition)) stop("Unsupported `condition_effects`.", call. = FALSE)
  if (!is.character(participant_effects) || any(!participant_effects %in% allowed_participant)) stop("Unsupported `participant_effects`.", call. = FALSE)
  data <- .p05_data(prepared)
  mapping <- .p05_mapping(prepared, require_condition = length(condition_effects) > 0L)
  if (!is.numeric(data[[mapping$response]])) stop("Pupil response must be numeric.", call. = FALSE)
  if (!is.numeric(data[[mapping$time]]) || any(!is.finite(data[[mapping$time]]))) stop("Time must be numeric, finite, and non-missing.", call. = FALSE)
  if (anyNA(data[[mapping$participant]])) stop("Participant identifiers must be non-missing.", call. = FALSE)
  if (length(condition_effects) > 0L) {
    n_condition <- length(unique(data[[mapping$condition]][!is.na(data[[mapping$condition]])]))
    if (n_condition < 2L) stop("Condition effects require at least two observed condition levels.", call. = FALSE)
  }
  protected <- unlist(mapping, use.names = FALSE)
  protected <- protected[!is.na(protected)]
  covariates <- .p05_check_covariates(data, covariates, protected)
  if (!is.null(prior_scales) && (!is.numeric(prior_scales) || is.null(names(prior_scales)) || any(prior_scales <= 0) || any(!is.finite(prior_scales)))) {
    stop("`prior_scales` must be NULL or a named positive finite vector.", call. = FALSE)
  }

  structure(
    list(
      version = "0.5.0.9000",
      prepared = prepared,
      data = data,
      mapping = mapping,
      family = family,
      condition_effects = unique(condition_effects),
      participant_effects = unique(participant_effects),
      covariates = covariates,
      prior_scales = prior_scales,
      experimental = TRUE,
      fit_performed = FALSE,
      governance = paste(
        "This response-shape model is experimental.",
        "Its amplitude, onset, rise, duration, and decay parameters are model parameters, not direct cognitive constructs."
      )
    ),
    class = "gp3bayes_pupil_response_shape_specification"
  )
}

.p05_shape_formula <- function(spec) {
  .p05_require("brms", "to translate the nonlinear response-shape model")
  m <- spec$mapping
  y <- .p05_q(m$response)
  t <- .p05_q(m$time)
  cond <- if (is.null(m$condition)) NULL else .p05_q(m$condition)
  p <- .p05_q(m$participant)

  main <- stats::as.formula(
    paste0(
      y, " ~ baseline + exp(logAmplitude) * ",
      "inv_logit((", t, " - onset) / exp(logRise)) * ",
      "inv_logit((onset + exp(logDuration) - ", t, ") / exp(logDecay))"
    )
  )

  baseline_terms <- c("1", .p05_q(spec$covariates))
  if ("baseline" %in% spec$participant_effects) baseline_terms <- c(baseline_terms, paste0("(1 | ", p, ")"))
  amp_terms <- "1"
  if ("amplitude" %in% spec$condition_effects && !is.null(cond)) amp_terms <- c(amp_terms, cond)
  if ("amplitude" %in% spec$participant_effects) amp_terms <- c(amp_terms, paste0("(1 | ", p, ")"))
  onset_terms <- "1"
  if ("onset" %in% spec$condition_effects && !is.null(cond)) onset_terms <- c(onset_terms, cond)
  duration_terms <- "1"
  if ("duration" %in% spec$condition_effects && !is.null(cond)) duration_terms <- c(duration_terms, cond)

  brms::bf(
    main,
    stats::as.formula(paste("baseline ~", .p05_rhs_join(baseline_terms))),
    stats::as.formula(paste("logAmplitude ~", .p05_rhs_join(amp_terms))),
    stats::as.formula(paste("onset ~", .p05_rhs_join(onset_terms))),
    logRise ~ 1,
    stats::as.formula(paste("logDuration ~", .p05_rhs_join(duration_terms))),
    logDecay ~ 1,
    nl = TRUE
  )
}

.p05_shape_priors <- function(spec, formula, data, family) {
  .p05_require("brms", "to create nonlinear priors")
  y <- data[[spec$mapping$response]]
  t <- data[[spec$mapping$time]]
  y_med <- stats::median(y, na.rm = TRUE)
  y_sd <- .p05_safe_sd(y)
  t_mid <- stats::median(t, na.rm = TRUE)
  t_span <- diff(range(t, na.rm = TRUE))
  if (!is.finite(t_span) || t_span <= 0) t_span <- 1000
  ov <- spec$prior_scales
  scl <- function(nm, d) if (!is.null(ov) && nm %in% names(ov)) unname(ov[[nm]]) else d

  intercept_prior <- c(
    baseline = sprintf("normal(%.12g, %.12g)", y_med, scl("baseline", y_sd)),
    logAmplitude = sprintf("normal(log(%.12g), %.12g)", max(0.1 * y_sd, 0.05), scl("log_amplitude", 0.8)),
    onset = sprintf("normal(%.12g, %.12g)", t_mid, scl("onset", t_span / 3)),
    logRise = sprintf("normal(log(%.12g), %.12g)", max(t_span / 12, 1), scl("log_rise", 0.7)),
    logDuration = sprintf("normal(log(%.12g), %.12g)", max(t_span / 2, 1), scl("log_duration", 0.7)),
    logDecay = sprintf("normal(log(%.12g), %.12g)", max(t_span / 8, 1), scl("log_decay", 0.7))
  )
  slope_scale <- c(
    baseline = scl("baseline_slope", y_sd),
    logAmplitude = scl("log_amplitude_slope", 0.5),
    onset = scl("onset_slope", t_span / 6),
    logRise = scl("log_rise_slope", 0.4),
    logDuration = scl("log_duration_slope", 0.4),
    logDecay = scl("log_decay_slope", 0.4)
  )
  group_scale <- c(
    baseline = scl("baseline_sd", y_sd),
    logAmplitude = scl("log_amplitude_sd", 0.5),
    onset = scl("onset_sd", t_span / 6),
    logRise = scl("log_rise_sd", 0.4),
    logDuration = scl("log_duration_sd", 0.4),
    logDecay = scl("log_decay_sd", 0.4)
  )

  available <- brms::get_prior(formula = formula, data = data, family = family)

  # Drop the brmsprior subclass only for ordinary inspection/subsetting.
  # Priors constructed below with set_prior() remain brmsprior objects.
  class(available) <- "data.frame"

  priors <- NULL
  add_prior <- function(p) {
    priors <<- if (is.null(priors)) p else c(priors, p)
  }

  # Use coefficient-specific population priors. Nonlinear intercepts encode the
  # natural location/scale of each shape parameter, whereas experimental
  # condition/covariate slopes are centered at zero.
  b_rows <- available[available$class == "b" & !is.na(available$nlpar) & nzchar(available$nlpar), , drop = FALSE]
  if (nrow(b_rows)) {
    key <- unique(b_rows[, intersect(c("coef", "nlpar"), names(b_rows)), drop = FALSE])
    for (i in seq_len(nrow(key))) {
      nl <- key$nlpar[[i]]
      cf <- key$coef[[i]]
      if (!nl %in% names(intercept_prior) || !nzchar(cf)) next
      prior_text <- if (identical(cf, "Intercept")) {
        intercept_prior[[nl]]
      } else {
        sprintf("normal(0, %.12g)", slope_scale[[nl]])
      }
      add_prior(brms::set_prior(prior_text, class = "b", coef = cf, nlpar = nl))
    }
  }

  # Add explicit proper priors for nonlinear group-level scales where present.
  sd_rows <- available[available$class == "sd" & !is.na(available$nlpar) & nzchar(available$nlpar), , drop = FALSE]
  if (nrow(sd_rows)) {
    for (nl in unique(sd_rows$nlpar)) {
      if (!nl %in% names(group_scale)) next
      add_prior(brms::set_prior(
        sprintf("student_t(3, 0, %.12g)", group_scale[[nl]]),
        class = "sd", nlpar = nl
      ))
    }
  }

  if (any(available$class == "sigma")) {
    add_prior(brms::set_prior(sprintf("student_t(3, 0, %.12g)", scl("sigma", y_sd)), class = "sigma"))
  }
  if (spec$family == "student" && any(available$class == "nu")) {
    add_prior(brms::set_prior(sprintf("gamma(2, %.12g)", scl("nu_rate", 0.1)), class = "nu"))
  }
  if (is.null(priors)) stop("Could not match any proper priors to the nonlinear response-shape formula.", call. = FALSE)
  priors
}

#' Translate the experimental response-shape model to brms
#'
#' @param specification A response-shape specification.
#' @return A backend-independent brms translation object.
#' @export
translate_pupil_response_shape_to_brms <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_response_shape_specification")) stop("Expected a response-shape specification.", call. = FALSE)
  .p05_require("brms", "to translate response-shape models")
  data <- specification$data
  data <- data[is.finite(data[[specification$mapping$response]]) & is.finite(data[[specification$mapping$time]]), , drop = FALSE]
  formula <- .p05_shape_formula(specification)
  family <- brms::brmsfamily(specification$family)
  priors <- .p05_shape_priors(specification, formula, data, family)
  structure(
    list(specification = specification, formula = formula, family = family, priors = priors, data = data),
    class = "gp3bayes_pupil_response_shape_brms_specification"
  )
}

#' Fit the experimental nonlinear response-shape model
#'
#' @param specification A response-shape specification.
#' @param backend rstan or cmdstanr.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted sampling controls.
#' @export
fit_pupil_response_shape_model <- function(
    specification,
    backend = c("rstan", "cmdstanr"),
    chains = 4L,
    iter = 2500L,
    warmup = 1250L,
    cores = min(2L, chains),
    seed = 2026,
    adapt_delta = 0.97,
    max_treedepth = 13L,
    refresh = 0L) {

  backend <- match.arg(backend)
  chains <- .p05_assert_integerish(chains, "chains", 1L, 16L)
  iter <- .p05_assert_integerish(iter, "iter", 100L, 1000000L)
  warmup <- .p05_assert_integerish(warmup, "warmup", 0L, iter - 1L)
  cores <- .p05_assert_integerish(cores, "cores", 1L, 2L)
  max_treedepth <- .p05_assert_integerish(max_treedepth, "max_treedepth", 8L, 20L)
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L || !is.finite(adapt_delta) || adapt_delta < 0.8 || adapt_delta >= 1) stop("`adapt_delta` must be in [0.8, 1).", call. = FALSE)
  tr <- translate_pupil_response_shape_to_brms(specification)
  fit <- brms::brm(
    formula = tr$formula, data = tr$data, family = tr$family, prior = tr$priors,
    backend = backend, chains = chains, iter = iter, warmup = warmup, cores = cores,
    seed = seed, control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    refresh = refresh, save_pars = brms::save_pars(all = TRUE)
  )
  structure(
    list(specification = specification, translation = tr, backend_fit = fit, backend = backend, fit_performed = TRUE),
    class = "gp3bayes_pupil_response_shape_fit"
  )
}

#' Estimate nonlinear response-shape parameters
#'
#' @param fit A response-shape fit.
#' @param probability Central interval probability.
#' @return A `gp3bayes_pupil_response_parameters` object.
#' @export
estimate_pupil_response_parameters <- function(fit, probability = 0.95) {
  if (!inherits(fit, "gp3bayes_pupil_response_shape_fit")) stop("Expected a response-shape fit.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  .p05_require("posterior", "to summarise response-shape draws")
  d <- posterior::as_draws_df(fit$backend_fit)
  vars <- names(d)[grepl("^(b_)?(baseline|logAmplitude|onset|logRise|logDuration|logDecay)", names(d))]
  if (!length(vars)) stop("Could not locate nonlinear response-shape parameters.", call. = FALSE)
  alpha <- (1 - probability) / 2
  tab <- .p05_quantile_summary(posterior::as_draws_matrix(posterior::subset_draws(d, variable = vars)), c(alpha, 0.5, 1 - alpha))
  tab$parameter <- vars
  tab$interpretation_scale <- ifelse(
    grepl("logAmplitude", vars), "log-amplitude coefficient",
    ifelse(grepl("logRise", vars), "log-rise coefficient",
      ifelse(grepl("logDuration", vars), "log-duration coefficient",
        ifelse(grepl("logDecay", vars), "log-decay coefficient", "native model scale")
      )
    )
  )
  tab <- tab[, c("parameter", "interpretation_scale", "mean", "sd", "q_low", "median", "q_high")]
  structure(
    list(table = tab, probability = probability, experimental = TRUE),
    class = "gp3bayes_pupil_response_parameters"
  )
}

#' Tabulate nonlinear response parameters
#' @param x A response-parameter object.
#' @export
pupil_response_parameter_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_response_parameters")) stop("Expected response parameters.", call. = FALSE)
  x$table
}

#' @export
print.gp3bayes_pupil_response_shape_specification <- function(x, ...) {
  cat("<gp3bayes_pupil_response_shape_specification>\n")
  cat("  EXPERIMENTAL: TRUE\n")
  cat("  Family:", x$family, "\n")
  cat("  Condition effects:", paste(x$condition_effects, collapse = ", "), "\n")
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_response_shape_fit <- function(x, ...) {
  cat("<gp3bayes_pupil_response_shape_fit>\n")
  cat("  EXPERIMENTAL: TRUE\n")
  cat("  Backend:", x$backend, "\n")
  cat("  Fit performed: TRUE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_response_parameters <- function(x, ...) {
  cat("<gp3bayes_pupil_response_parameters>\n")
  cat("  EXPERIMENTAL: TRUE\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_response_parameters <- function(x, row.names = NULL, optional = FALSE, ...) x$table

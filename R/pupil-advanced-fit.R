# Translation and fitting for gp3bayes 0.5 advanced pupil models.

.p05_resolve_common_alias <- function(data, alias) {
  candidates <- switch(
    alias,
    baseline = c("baseline", "baseline_pupil", "pupil_baseline", "baseline_value"),
    luminance = c("luminance", "screen_luminance", "stimulus_luminance", "luminance_value"),
    gaze_eccentricity = c("gaze_eccentricity", "eccentricity", "gaze_distance", "gaze_ecc"),
    alias
  )
  hit <- candidates[candidates %in% names(data)]
  if (!length(hit)) return(NULL)
  hit[[1L]]
}

.p05_measurement_map <- function(spec, data) {
  mm <- spec$measurement_model
  if (is.null(mm)) return(character())
  out <- character()
  for (nm in names(mm$covariate_errors)) {
    resolved <- if (nm %in% names(data)) nm else .p05_resolve_common_alias(data, nm)
    if (is.null(resolved)) {
      stop("Could not resolve measurement-error covariate alias `", nm, "` in the data.", call. = FALSE)
    }
    out[[resolved]] <- unname(mm$covariate_errors[[nm]])
  }
  out
}

.p05_main_covariate_terms <- function(spec, measurement_map, missing_predictors) {
  covariates <- spec$covariates
  if (!length(covariates)) return(character())
  vapply(
    covariates,
    function(z) {
      if (z %in% c(names(measurement_map), missing_predictors)) {
        paste0("mi(", .p05_q(z), ")")
      } else {
        .p05_q(z)
      }
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

.p05_mean_rhs <- function(spec, data, measurement_map = character(), missing_predictors = character()) {
  m <- spec$mapping
  t <- .p05_q(m$time)
  cond <- if (is.null(m$condition)) NULL else .p05_q(m$condition)
  participant <- .p05_q(m$participant)
  item <- if (is.null(m$item)) NULL else .p05_q(m$item)
  k <- spec$smooth_basis_dimension

  terms <- character()

  if (spec$temporal_structure == "linear") {
    terms <- c(terms, t)
    if (!is.null(cond)) {
      terms <- c(terms, cond)
      if (spec$condition_trajectory) terms <- c(terms, paste0(cond, ":", t))
    }
  }

  if (spec$temporal_structure == "smooth") {
    if (!is.null(cond)) terms <- c(terms, cond)
    terms <- c(terms, paste0("s(", t, ", k = ", k, ")"))
    if (!is.null(cond) && spec$condition_trajectory) {
      terms <- c(terms, paste0("s(", t, ", by = ", cond, ", k = ", k, ")"))
    }
  }

  if (spec$temporal_structure == "gaussian_process") {
    gp <- spec$gp_spec
    karg <- if (gp$basis == "exact") "NA" else as.character(gp$k)
    if (!is.null(cond)) terms <- c(terms, cond)
    base_gp <- paste0(
      "gp(", t,
      ", k = ", karg,
      ", cov = \"", gp$kernel, "\"",
      ", scale = ", if (gp$scale) "TRUE" else "FALSE",
      ")"
    )
    if (!is.null(cond) && spec$condition_trajectory) {
      base_gp <- paste0(
        "gp(", t,
        ", by = ", cond,
        ", k = ", karg,
        ", cov = \"", gp$kernel, "\"",
        ", scale = ", if (gp$scale) "TRUE" else "FALSE",
        ")"
      )
    }
    terms <- c(terms, base_gp)
  }

  terms <- c(terms, .p05_main_covariate_terms(spec, measurement_map, missing_predictors))
  terms <- c(terms, paste0("(1 | ", participant, ")"))

  if (spec$participant_trajectory == "factor_smooth") {
    # mgcv factor-smooth basis, passed through brms smooth syntax.
    terms <- c(
      terms,
      paste0(
        "s(", t, ", ", participant,
        ", bs = \"fs\", k = ", max(4L, min(8L, k)), ")"
      )
    )
  }

  if (spec$item_effects && !is.null(item)) {
    terms <- c(terms, paste0("(1 | ", item, ")"))
  }

  .p05_rhs_join(terms)
}

.p05_sigma_formula <- function(spec) {
  if (spec$residual_scale == "constant") return(NULL)
  m <- spec$mapping
  t <- .p05_q(m$time)
  cond <- if (is.null(m$condition)) NULL else .p05_q(m$condition)
  k <- max(4L, min(spec$smooth_basis_dimension, 12L))

  if (spec$residual_scale %in% c("condition", "condition_time") && is.null(cond)) {
    stop("Condition-dependent residual scale requested but no condition column is available.", call. = FALSE)
  }

  rhs <- switch(
    spec$residual_scale,
    condition = paste0("~ 1 + ", cond),
    time = paste0("~ 1 + s(", t, ", k = ", k, ")"),
    condition_time = paste0(
      "~ 1 + ", cond,
      " + s(", t, ", k = ", k, ")",
      " + s(", t, ", by = ", cond, ", k = ", k, ")"
    )
  )
  stats::as.formula(rhs)
}

.p05_response_lhs <- function(spec) {
  y <- .p05_q(spec$mapping$response)
  response_error <- if (is.null(spec$measurement_model)) NULL else spec$measurement_model$response_error
  model_missing <- !is.null(spec$missingness_model) && identical(spec$missingness_model$response, "model")

  # brms' response-mi term can carry known response measurement uncertainty
  # through its sdy argument.  Use that single coherent response addition when
  # both missingness and known response error are declared instead of stacking
  # two independent response additions.
  if (model_missing && !is.null(response_error)) {
    return(paste0(y, " | mi(sdy = ", .p05_q(response_error), ")"))
  }
  if (model_missing) {
    return(paste0(y, " | mi()"))
  }
  if (!is.null(response_error)) {
    return(paste0(y, " | se(", .p05_q(response_error), ", sigma = TRUE)"))
  }
  y
}

.p05_latent_predictor_formulas <- function(spec, data, measurement_map, missing_predictors) {
  vars <- unique(c(names(measurement_map), missing_predictors))
  if (!length(vars)) return(list())
  m <- spec$mapping
  participant <- .p05_q(m$participant)
  cond <- if (is.null(m$condition)) NULL else .p05_q(m$condition)
  aux <- if (is.null(spec$missingness_model)) character() else spec$missingness_model$auxiliary_predictors
  aux <- setdiff(aux, vars)
  aux_terms <- .p05_q(aux)

  lapply(
    vars,
    function(v) {
      lhs <- .p05_q(v)
      if (v %in% names(measurement_map)) {
        lhs <- paste0(lhs, " | mi(", .p05_q(unname(measurement_map[[v]])), ")")
      } else {
        lhs <- paste0(lhs, " | mi()")
      }
      rhs_terms <- c("1", cond, aux_terms, paste0("(1 | ", participant, ")"))
      stats::as.formula(paste(lhs, "~", .p05_rhs_join(rhs_terms)))
    }
  )
}

.p05_build_brms_formula <- function(spec, data) {
  .p05_require("brms", "to translate advanced pupil specifications")
  measurement_map <- .p05_measurement_map(spec, data)
  missing_predictors <- if (is.null(spec$missingness_model)) character() else spec$missingness_model$predictors

  rhs <- .p05_mean_rhs(spec, data, measurement_map, missing_predictors)
  main_formula <- stats::as.formula(paste(.p05_response_lhs(spec), "~", rhs))
  sigma_formula <- .p05_sigma_formula(spec)

  main_bf <- if (is.null(sigma_formula)) {
    brms::bf(main_formula)
  } else {
    brms::bf(main_formula, sigma = sigma_formula)
  }

  latent <- .p05_latent_predictor_formulas(spec, data, measurement_map, missing_predictors)
  out <- main_bf
  if (length(latent)) {
    for (f in latent) out <- out + brms::bf(f)
    out <- out + brms::set_rescor(FALSE)
  }

  if (!is.null(spec$autocorrelation)) {
    ac <- spec$autocorrelation
    ac_formula <- stats::as.formula(
      paste0(
        "~ arma(time = .gp3bayes_time_index, gr = .gp3bayes_series, p = ", ac$p,
        ", q = ", ac$q,
        ", cov = ", if (ac$covariance) "TRUE" else "FALSE", ")"
      )
    )
    # If latent predictor submodels are present, the formula is multivariate.
    # Scope the residual autocorrelation explicitly to the pupil response so it
    # is not accidentally inherited by auxiliary latent-response equations.
    out <- out + brms::acformula(ac_formula, resp = spec$mapping$response)
  }

  out
}

.p05_family_object <- function(spec, n_latent = 0L) {
  .p05_require("brms", "to create a brms family")

  n_latent <- .p05_assert_integerish(
    n_latent,
    "n_latent",
    0L,
    1000L
  )

  main <- brms::brmsfamily(
    family = spec$family
  )

  if (n_latent == 0L) {
    return(main)
  }

  latent_family <- brms::brmsfamily(
    family = "gaussian"
  )

  c(
    list(main),
    rep(
      list(latent_family),
      n_latent
    )
  )
}

#' Create an advanced pupil prior specification
#'
#' Constructs the governed default prior specification used when
#' translating an advanced pupil time-course model to `brms`.
#' This function defines prior scales only; it does not compile or fit
#' a model and does not establish model adequacy.
#'
#' @param specification A `gp3bayes_pupil_advanced_specification`
#'   created by [specify_advanced_pupil_timecourse_model()].
#'
#' @return A `gp3bayes_pupil_advanced_prior_specification` object.
#' @export
create_advanced_pupil_prior_specification <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) {
    stop("`specification` must be an advanced pupil specification.", call. = FALSE)
  }
  y <- specification$data[[specification$mapping$response]]
  y_med <- stats::median(y, na.rm = TRUE)
  y_sd <- .p05_safe_sd(y)
  overrides <- specification$prior_scales
  get_scale <- function(nm, default) {
    if (!is.null(overrides) && nm %in% names(overrides)) unname(overrides[[nm]]) else default
  }

  out <- list(
    intercept_location = y_med,
    intercept_scale = get_scale("intercept", 2 * y_sd),
    coefficient_scale = get_scale("b", y_sd),
    group_sd_scale = get_scale("sd", y_sd),
    smooth_sd_scale = get_scale("sds", y_sd),
    gp_sd_scale = get_scale("sdgp", y_sd),
    sigma_scale = get_scale("sigma", y_sd),
    sigma_coefficient_scale = get_scale("sigma_b", 0.5),
    nu_rate = get_scale("nu_rate", 0.1),
    ar_scale = get_scale("ar", 0.35),
    ma_scale = get_scale("ma", 0.35)
  )

  structure(out, class = "gp3bayes_pupil_advanced_prior_specification")
}

.p05_prior_candidates <- function(priorspec) {
  data.frame(
    class = c("Intercept", "b", "sd", "sds", "sdgp", "sigma", "b", "Intercept", "nu", "ar", "ma"),
    dpar = c("", "", "", "", "", "", "sigma", "sigma", "", "", ""),
    prior = c(
      sprintf("normal(%.12g, %.12g)", priorspec$intercept_location, priorspec$intercept_scale),
      sprintf("normal(0, %.12g)", priorspec$coefficient_scale),
      sprintf("student_t(3, 0, %.12g)", priorspec$group_sd_scale),
      sprintf("student_t(3, 0, %.12g)", priorspec$smooth_sd_scale),
      sprintf("student_t(3, 0, %.12g)", priorspec$gp_sd_scale),
      sprintf("student_t(3, 0, %.12g)", priorspec$sigma_scale),
      sprintf("normal(0, %.12g)", priorspec$sigma_coefficient_scale),
      "normal(0, 1)",
      sprintf("gamma(2, %.12g)", priorspec$nu_rate),
      sprintf("normal(0, %.12g)", priorspec$ar_scale),
      sprintf("normal(0, %.12g)", priorspec$ma_scale)
    ),
    stringsAsFactors = FALSE
  )
}

.p05_response_data_column <- function(resp, data) {
  if (is.null(resp) || is.na(resp) || !nzchar(resp)) return(NULL)
  if (resp %in% names(data)) return(resp)
  norm <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
  hit <- names(data)[norm(names(data)) == norm(resp)]
  if (length(hit) == 1L) hit[[1L]] else NULL
}

.p05_add_prior <- function(current, prior) {
  if (is.null(current)) prior else c(current, prior)
}

.p05_match_priors <- function(formula, data, family, priorspec, specification) {
  .p05_require("brms", "to validate advanced priors")
  available <- brms::get_prior(formula = formula, data = data, family = family)
  if (!nrow(available)) return(NULL)
  priors <- NULL

  # brms formulas that include mi() predictor submodels are multivariate. Build
  # priors per response so latent baseline/luminance equations are not assigned
  # pupil-diameter intercept/scale priors. If no response label is present, the
  # ordinary univariate prior specification is used.
  response_labels <- if ("resp" %in% names(available)) {
    unique(available$resp[!is.na(available$resp) & nzchar(available$resp)])
  } else {
    character()
  }

  if (!length(response_labels)) {
    cand <- .p05_prior_candidates(priorspec)
    for (i in seq_len(nrow(cand))) {
      cls <- cand$class[[i]]
      dp <- cand$dpar[[i]]
      hit <- available$class == cls
      if (nzchar(dp) && "dpar" %in% names(available)) hit <- hit & !is.na(available$dpar) & available$dpar == dp
      if (!nzchar(dp) && "dpar" %in% names(available)) hit <- hit & (is.na(available$dpar) | available$dpar == "")
      if (!any(hit, na.rm = TRUE)) next
      pp <- if (nzchar(dp)) {
        brms::set_prior(cand$prior[[i]], class = cls, dpar = dp)
      } else {
        brms::set_prior(cand$prior[[i]], class = cls)
      }
      priors <- .p05_add_prior(priors, pp)
    }
    return(priors)
  }

  main_resp <- specification$mapping$response
  main_norm <- tolower(gsub("[^[:alnum:]]", "", main_resp))

  for (resp in response_labels) {
    col <- .p05_response_data_column(resp, data)
    z <- if (!is.null(col) && is.numeric(data[[col]])) data[[col]] else data[[main_resp]]
    loc <- stats::median(z, na.rm = TRUE)
    sc <- .p05_safe_sd(z)
    is_main <- identical(tolower(gsub("[^[:alnum:]]", "", resp)), main_norm)

    rows <- available[!is.na(available$resp) & available$resp == resp, , drop = FALSE]
    classes <- unique(rows$class)

    if ("Intercept" %in% classes) {
      pp <- brms::set_prior(
        sprintf("normal(%.12g, %.12g)", loc, if (is_main) priorspec$intercept_scale else 2 * sc),
        class = "Intercept", resp = resp
      )
      priors <- .p05_add_prior(priors, pp)
    }
    if ("b" %in% classes) {
      # Distributional sigma coefficients are handled separately below. This
      # broad response-level prior applies to ordinary population coefficients.
      pp <- brms::set_prior(
        sprintf("normal(0, %.12g)", if (is_main) priorspec$coefficient_scale else sc),
        class = "b", resp = resp
      )
      priors <- .p05_add_prior(priors, pp)
    }
    for (cls in intersect(c("sd", "sds", "sdgp"), classes)) {
      sc_use <- if (is_main) {
        switch(cls, sd = priorspec$group_sd_scale, sds = priorspec$smooth_sd_scale, sdgp = priorspec$gp_sd_scale)
      } else {
        sc
      }
      priors <- .p05_add_prior(
        priors,
        brms::set_prior(sprintf("student_t(3, 0, %.12g)", sc_use), class = cls, resp = resp)
      )
    }
    if ("sigma" %in% classes) {
      priors <- .p05_add_prior(
        priors,
        brms::set_prior(
          sprintf("student_t(3, 0, %.12g)", if (is_main) priorspec$sigma_scale else sc),
          class = "sigma", resp = resp
        )
      )
    }
    if ("nu" %in% classes) {
      priors <- .p05_add_prior(
        priors,
        brms::set_prior(sprintf("gamma(2, %.12g)", priorspec$nu_rate), class = "nu", resp = resp)
      )
    }
    if (is_main && "ar" %in% classes) {
      priors <- .p05_add_prior(priors, brms::set_prior(sprintf("normal(0, %.12g)", priorspec$ar_scale), class = "ar", resp = resp))
    }
    if (is_main && "ma" %in% classes) {
      priors <- .p05_add_prior(priors, brms::set_prior(sprintf("normal(0, %.12g)", priorspec$ma_scale), class = "ma", resp = resp))
    }
  }

  # Distributional sigma belongs to the main response. brms represents these
  # population coefficients via dpar = sigma rather than a separate response.
  if ("dpar" %in% names(available)) {
    sigma_rows <- available[
      available$class %in% c("b", "Intercept") & !is.na(available$dpar) & available$dpar == "sigma",
      , drop = FALSE
    ]
    if (nrow(sigma_rows)) {
      resp_arg <- unique(sigma_rows$resp[!is.na(sigma_rows$resp) & nzchar(sigma_rows$resp)])
      resp_arg <- if (length(resp_arg) == 1L) resp_arg[[1L]] else NULL
      if (any(sigma_rows$class == "b")) {
        args <- list(
          prior = sprintf("normal(0, %.12g)", priorspec$sigma_coefficient_scale),
          class = "b", dpar = "sigma"
        )
        if (!is.null(resp_arg)) args$resp <- resp_arg
        priors <- .p05_add_prior(priors, do.call(brms::set_prior, args))
      }
      if (any(sigma_rows$class == "Intercept")) {
        args <- list(prior = "normal(0, 1)", class = "Intercept", dpar = "sigma")
        if (!is.null(resp_arg)) args$resp <- resp_arg
        priors <- .p05_add_prior(priors, do.call(brms::set_prior, args))
      }
    }
  }

  priors
}

#' Translate an advanced pupil model to brms without fitting
#'
#' @param specification An advanced pupil specification.
#' @return A `gp3bayes_pupil_advanced_brms_specification` object containing the
#'   brms formula, family, priors, and translated data.
#' @export
translate_advanced_pupil_model_to_brms <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) {
    stop("`specification` must be an advanced pupil specification.", call. = FALSE)
  }
  .p05_require("brms", "to translate advanced pupil specifications")

  data <- .p05_series_data(specification$data, specification$mapping)

  # Response exclusion is explicit and happens only at translation, never by
  # silently interpolating values.
  if (!is.null(specification$missingness_model) && specification$missingness_model$response == "exclude") {
    data <- data[!is.na(data[[specification$mapping$response]]), , drop = FALSE]
  } else if (is.null(specification$missingness_model)) {
    data <- data[!is.na(data[[specification$mapping$response]]), , drop = FALSE]
  }

  measurement_map <- .p05_measurement_map(specification, data)
  missing_predictors <- if (is.null(specification$missingness_model)) character() else specification$missingness_model$predictors
  latent_variables <- unique(c(names(measurement_map), missing_predictors))

  formula <- .p05_build_brms_formula(specification, data)
  family <- .p05_family_object(specification, n_latent = length(latent_variables))
  prior_spec <- create_advanced_pupil_prior_specification(specification)
  priors <- .p05_match_priors(formula, data, family, prior_spec, specification)

  structure(
    list(
      specification = specification,
      formula = formula,
      family = family,
      priors = priors,
      prior_specification = prior_spec,
      data = data,
      backend = "none",
      compiled = FALSE,
      fit_performed = FALSE
    ),
    class = "gp3bayes_pupil_advanced_brms_specification"
  )
}

#' Check advanced pupil priors through prior-only simulation
#'
#' This optional backend gate samples only from priors using brms. It does not
#' establish model adequacy.
#'
#' @param specification An advanced specification.
#' @param backend `"rstan"` or `"cmdstanr"`.
#' @param chains,iter,warmup,cores,seed Sampling controls.
#' @return A `gp3bayes_pupil_advanced_prior_predictive` object.
#' @export
check_advanced_pupil_prior_predictive <- function(
    specification,
    backend = c("rstan", "cmdstanr"),
    chains = 2L,
    iter = 800L,
    warmup = 400L,
    cores = min(2L, chains),
    seed = 2026) {

  backend <- match.arg(backend)
  chains <- .p05_assert_integerish(chains, "chains", 1L, 16L)
  iter <- .p05_assert_integerish(iter, "iter", 100L, 1000000L)
  warmup <- .p05_assert_integerish(warmup, "warmup", 0L, iter - 1L)
  cores <- .p05_assert_integerish(cores, "cores", 1L, 2L)
  tr <- translate_advanced_pupil_model_to_brms(specification)
  .p05_require("brms", "for prior predictive sampling")

  fit <- brms::brm(
    formula = tr$formula,
    data = tr$data,
    family = tr$family,
    prior = tr$priors,
    backend = backend,
    sample_prior = "only",
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    refresh = 0
  )

  yrep <- brms::posterior_predict(fit, ndraws = min(200L, iter - warmup))
  observed <- tr$data[[specification$mapping$response]]
  finite_draw <- apply(yrep, 1L, function(z) all(is.finite(z)))
  obs_range <- range(observed, na.rm = TRUE)
  span <- diff(obs_range)
  if (!is.finite(span) || span <= 0) span <- .p05_safe_sd(observed)
  extreme <- apply(yrep, 1L, function(z) any(z < obs_range[[1L]] - 10 * span | z > obs_range[[2L]] + 10 * span))

  structure(
    list(
      finite_fraction = mean(finite_draw),
      extreme_fraction = mean(extreme),
      draws = nrow(yrep),
      backend = backend,
      fit = fit,
      interpretation = "Prior predictive simulation is a plausibility diagnostic, not proof of adequacy."
    ),
    class = "gp3bayes_pupil_advanced_prior_predictive"
  )
}

#' Fit an advanced pupil model through an approved brms backend
#'
#' @param specification An advanced pupil specification.
#' @param backend `"rstan"` or `"cmdstanr"`.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted MCMC controls.
#' @return A `gp3bayes_pupil_advanced_fit` object.
#' @export
fit_advanced_pupil_model_backend <- function(
    specification,
    backend = c("rstan", "cmdstanr"),
    chains = 4L,
    iter = 2000L,
    warmup = 1000L,
    cores = min(2L, chains),
    seed = 2026,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L) {

  backend <- match.arg(backend)
  chains <- .p05_assert_integerish(chains, "chains", 1L, 16L)
  iter <- .p05_assert_integerish(iter, "iter", 100L, 1000000L)
  warmup <- .p05_assert_integerish(warmup, "warmup", 0L, iter - 1L)
  cores <- .p05_assert_integerish(cores, "cores", 1L, 2L)
  max_treedepth <- .p05_assert_integerish(max_treedepth, "max_treedepth", 8L, 20L)
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L || !is.finite(adapt_delta) || adapt_delta < 0.8 || adapt_delta >= 1) stop("`adapt_delta` must be in [0.8, 1).", call. = FALSE)
  tr <- translate_advanced_pupil_model_to_brms(specification)
  .p05_require("brms", "to fit advanced pupil models")

  fit <- brms::brm(
    formula = tr$formula,
    data = tr$data,
    family = tr$family,
    prior = tr$priors,
    backend = backend,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    refresh = as.integer(refresh),
    save_pars = brms::save_pars(all = TRUE)
  )

  structure(
    list(
      specification = specification,
      translation = tr,
      backend_fit = fit,
      backend = backend,
      fit_performed = TRUE,
      sampling = list(
        chains = chains, iter = iter, warmup = warmup, cores = cores,
        seed = seed, adapt_delta = adapt_delta, max_treedepth = max_treedepth
      ),
      interpretation = paste(
        "A returned fit does not establish convergence, model adequacy, causal identification,",
        "or substantive interpretation. Run diagnostics, predictive checks, validation, and sensitivity analyses."
      )
    ),
    class = c("gp3bayes_pupil_advanced_fit", "gp3bayes_pupil_fit")
  )
}

#' Fit an advanced pupil model using rstan
#' @inheritParams fit_advanced_pupil_model_backend
#' @export
fit_advanced_pupil_model <- function(
    specification,
    chains = 4L,
    iter = 2000L,
    warmup = 1000L,
    cores = min(2L, chains),
    seed = 2026,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L) {
  fit_advanced_pupil_model_backend(
    specification = specification, backend = "rstan", chains = chains,
    iter = iter, warmup = warmup, cores = cores, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, refresh = refresh
  )
}

#' Fit an advanced pupil model using cmdstanr
#' @inheritParams fit_advanced_pupil_model_backend
#' @export
fit_advanced_pupil_model_cmdstanr <- function(
    specification,
    chains = 4L,
    iter = 2000L,
    warmup = 1000L,
    cores = min(2L, chains),
    seed = 2026,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L) {
  fit_advanced_pupil_model_backend(
    specification = specification, backend = "cmdstanr", chains = chains,
    iter = iter, warmup = warmup, cores = cores, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, refresh = refresh
  )
}

#' @export
print.gp3bayes_pupil_advanced_brms_specification <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_brms_specification>\n")
  cat("  Family:", x$specification$family, "\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Formula:\n")
  print(x$formula)
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_advanced_fit <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_fit>\n")
  cat("  Backend:", x$backend, "\n")
  cat("  Family:", x$specification$family, "\n")
  cat("  Temporal structure:", x$specification$temporal_structure, "\n")
  cat("  Residual scale:", x$specification$residual_scale, "\n")
  cat("  Fit performed: TRUE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_advanced_prior_predictive <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_prior_predictive>\n")
  cat("  Backend:", x$backend, "\n")
  cat("  Draws:", x$draws, "\n")
  cat("  Finite-draw fraction:", format(x$finite_fraction, digits = 3), "\n")
  cat("  Extreme-draw fraction:", format(x$extreme_fraction, digits = 3), "\n")
  invisible(x)
}

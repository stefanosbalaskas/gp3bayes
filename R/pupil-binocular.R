# Joint binocular pupil modelling for gp3bayes 0.5.

#' Prepare binocular pupil data without averaging eyes
#'
#' @param data A data frame containing left and right pupil responses.
#' @param left_col,right_col Left/right pupil response columns.
#' @param participant_col,time_col,condition_col Structural columns.
#' @param trial_col,item_col Optional structural columns.
#' @param covariates Optional additional covariates.
#' @return A `gp3bayes_binocular_pupil_prepared` object.
#' @export
prepare_binocular_pupil_timecourse <- function(
    data,
    left_col = "pupil_left",
    right_col = "pupil_right",
    participant_col = "participant_id",
    time_col = "time_ms",
    condition_col = "condition",
    trial_col = "trial_id",
    item_col = NULL,
    covariates = character()) {

  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  required <- c(left_col, right_col, participant_col, time_col, condition_col)
  if (!is.null(trial_col)) required <- c(required, trial_col)
  if (!is.null(item_col)) required <- c(required, item_col)
  miss <- setdiff(c(required, covariates), names(data))
  if (length(miss)) stop("Missing binocular column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(data[[left_col]]) || !is.numeric(data[[right_col]])) stop("Left/right pupil responses must be numeric.", call. = FALSE)
  if (!is.numeric(data[[time_col]])) stop("Time must be numeric.", call. = FALSE)
  if (identical(left_col, right_col)) stop("Left and right response columns must differ.", call. = FALSE)
  if (anyNA(data[[participant_col]])) stop("Participant identifiers must be non-missing.", call. = FALSE)
  if (any(!is.finite(data[[time_col]]))) stop("Time values must be finite and non-missing.", call. = FALSE)
  if (!is.null(trial_col) && anyNA(data[[trial_col]])) stop("Trial identifiers must be non-missing when supplied.", call. = FALSE)
  if (!is.null(item_col) && anyNA(data[[item_col]])) stop("Item identifiers must be non-missing when supplied.", call. = FALSE)
  if (length(unique(data[[condition_col]][!is.na(data[[condition_col]])])) < 2L) {
    stop("The governed binocular model requires at least two observed condition levels.", call. = FALSE)
  }

  mapping <- list(
    left = left_col,
    right = right_col,
    participant = participant_col,
    time = time_col,
    condition = condition_col,
    trial = trial_col,
    item = item_col
  )
  structure(
    list(
      data = data,
      mapping = mapping,
      covariates = unique(covariates),
      fit_performed = FALSE,
      governance = "Left and right eyes remain distinct responses; no automatic averaging or eye substitution is performed."
    ),
    class = "gp3bayes_binocular_pupil_prepared"
  )
}

#' Audit binocular pupil availability and agreement descriptively
#'
#' @param prepared A binocular prepared object.
#' @return A `gp3bayes_binocular_pupil_audit` object.
#' @export
audit_binocular_pupil_readiness <- function(prepared) {
  if (!inherits(prepared, "gp3bayes_binocular_pupil_prepared")) stop("Expected binocular prepared data.", call. = FALSE)
  d <- prepared$data
  m <- prepared$mapping
  l <- d[[m$left]]
  r <- d[[m$right]]
  both <- is.finite(l) & is.finite(r)
  diff <- r[both] - l[both]
  tab <- data.frame(
    metric = c(
      "rows", "left_available_fraction", "right_available_fraction",
      "both_available_fraction", "mean_right_minus_left", "sd_right_minus_left",
      "pearson_correlation"
    ),
    value = c(
      nrow(d), mean(is.finite(l)), mean(is.finite(r)), mean(both),
      if (any(both)) mean(diff) else NA_real_,
      if (sum(both) > 1L) stats::sd(diff) else NA_real_,
      if (sum(both) > 2L) stats::cor(l[both], r[both]) else NA_real_
    ),
    stringsAsFactors = FALSE
  )
  status <- if (mean(both) < 0.3) "failure" else if (mean(both) < 0.7) "review" else "pass"
  structure(
    list(
      table = tab,
      status = status,
      interpretation = "Raw binocular agreement is descriptive and does not replace a joint measurement model."
    ),
    class = "gp3bayes_binocular_pupil_audit"
  )
}

.p05_binocular_rhs <- function(spec, response_name) {
  p <- spec$prepared
  d <- p$data
  m <- p$mapping
  t <- .p05_q(m$time)
  cond <- .p05_q(m$condition)
  participant <- .p05_q(m$participant)
  item <- if (is.null(m$item)) NULL else .p05_q(m$item)
  k <- spec$smooth_basis_dimension

  terms <- switch(
    spec$temporal_structure,
    linear = c(cond, t, paste0(cond, ":", t)),
    smooth = c(cond, paste0("s(", t, ", k = ", k, ")"), paste0("s(", t, ", by = ", cond, ", k = ", k, ")")),
    gaussian_process = {
      gp <- spec$gp_spec
      karg <- if (gp$basis == "exact") "NA" else as.character(gp$k)
      c(cond, paste0("gp(", t, ", by = ", cond, ", k = ", karg, ", cov = \"", gp$kernel, "\", scale = ", if (gp$scale) "TRUE" else "FALSE", ")"))
    }
  )
  terms <- c(terms, .p05_q(p$covariates), paste0("(1 | ", participant, ")"))
  if (spec$item_effects && !is.null(item)) terms <- c(terms, paste0("(1 | ", item, ")"))
  stats::as.formula(paste(.p05_q(response_name), "~", .p05_rhs_join(terms)))
}

#' Specify a joint binocular pupil model
#'
#' @param prepared A binocular prepared object.
#' @param temporal_structure Mean trajectory type.
#' @param family Gaussian or Student-t for both eyes.
#' @param smooth_basis_dimension Requested smooth basis dimension; when omitted, an unsupported default is conservatively reduced to observed temporal support, while explicitly unsupported values are rejected.
#' @param gp_spec GP configuration when requested.
#' @param residual_correlation Whether to estimate left/right residual correlation.
#' @param item_effects Include item random intercepts when available.
#' @param prior_scales Optional prior-scale overrides.
#' @param allow_high_complexity Explicit computational opt-in for exact GP on
#'   large time grids.
#' @return A `gp3bayes_binocular_pupil_specification` object.
#' @export
specify_binocular_pupil_model <- function(
    prepared,
    temporal_structure = c("smooth", "linear", "gaussian_process"),
    family = c("gaussian", "student"),
    smooth_basis_dimension = 10L,
    gp_spec = create_pupil_gp_spec(),
    residual_correlation = TRUE,
    item_effects = NULL,
    prior_scales = NULL,
    allow_high_complexity = FALSE) {

  smooth_k_was_missing <- missing(smooth_basis_dimension)

  if (!inherits(prepared, "gp3bayes_binocular_pupil_prepared")) {
    stop(
      "Expected output from prepare_binocular_pupil_timecourse().",
      call. = FALSE
    )
  }

  temporal_structure <- match.arg(temporal_structure)
  family <- match.arg(family)

  .p05_assert_flag(
    residual_correlation,
    "residual_correlation"
  )

  .p05_assert_flag(
    allow_high_complexity,
    "allow_high_complexity"
  )

  requested_k <- .p05_assert_integerish(
    smooth_basis_dimension,
    "smooth_basis_dimension",
    4L,
    100L
  )

  effective_k <- requested_k
  support_k <- NA_integer_
  basis_adjusted <- FALSE

  if (temporal_structure == "smooth") {

    d <- prepared$data
    m <- prepared$mapping

    time_value <- d[[m$time]]
    condition_value <- d[[m$condition]]

    observed <-
      is.finite(time_value) &
      !is.na(condition_value)

    global_support <- length(
      unique(
        time_value[observed]
      )
    )

    by_condition_support <- tapply(
      time_value[observed],
      condition_value[observed],
      function(z) {
        length(
          unique(
            z[is.finite(z)]
          )
        )
      }
    )

    by_condition_support <- as.integer(
      by_condition_support[
        is.finite(by_condition_support)
      ]
    )

    if (!length(by_condition_support)) {
      stop(
        "Could not determine condition-specific time support for the binocular smooth model.",
        call. = FALSE
      )
    }

    time_support <- min(
      c(
        global_support,
        by_condition_support
      )
    )

    # Keep one degree of support in reserve. This is intentionally
    # conservative because the formula contains both an overall smooth
    # and condition-specific by-factor smooths.
    support_k <- as.integer(
      time_support - 1L
    )

    if (support_k < 4L) {
      stop(
        paste0(
          "The binocular smooth model has insufficient temporal support. ",
          "At least five unique time values are required within every ",
          "observed condition; the smallest observed support is ",
          time_support,
          "."
        ),
        call. = FALSE
      )
    }

    if (requested_k > support_k) {

      if (smooth_k_was_missing) {

        effective_k <- support_k
        basis_adjusted <- TRUE

      } else {

        stop(
          paste0(
            "`smooth_basis_dimension` = ",
            requested_k,
            " exceeds the governed support of this binocular design. ",
            "The maximum supported value is ",
            support_k,
            " based on ",
            time_support,
            " unique time values in the least-supported condition. ",
            "Specify a smaller basis dimension explicitly."
          ),
          call. = FALSE
        )
      }
    }
  }

  if (
    temporal_structure == "gaussian_process" &&
    !inherits(gp_spec, "gp3bayes_pupil_gp_spec")
  ) {
    stop(
      "`gp_spec` must come from create_pupil_gp_spec().",
      call. = FALSE
    )
  }

  if (is.null(item_effects)) {
    item_effects <- !is.null(
      prepared$mapping$item
    )
  }

  .p05_assert_flag(
    item_effects,
    "item_effects"
  )

  if (!is.null(prior_scales)) {
    if (
      !is.numeric(prior_scales) ||
      is.null(names(prior_scales)) ||
      any(!is.finite(prior_scales)) ||
      any(prior_scales <= 0)
    ) {
      stop(
        "`prior_scales` must be NULL or a named positive finite numeric vector.",
        call. = FALSE
      )
    }
  }

  n_time <- length(
    unique(
      prepared$data[[
        prepared$mapping$time
      ]]
    )
  )

  if (
    temporal_structure == "gaussian_process" &&
    gp_spec$basis == "exact" &&
    n_time > 500L &&
    !allow_high_complexity
  ) {
    stop(
      paste(
        "Exact binocular GP exceeds the default complexity budget.",
        "Use an approximate GP or explicit",
        "`allow_high_complexity = TRUE`."
      ),
      call. = FALSE
    )
  }

  governance_text <- paste(
    "Joint modelling preserves eye-specific responses;",
    "residual correlation is an association parameter,",
    "not evidence of eye interchangeability."
  )

  if (basis_adjusted) {
    governance_text <- paste(
      governance_text,
      paste0(
        "The omitted default smooth basis was reduced from ",
        requested_k,
        " to ",
        effective_k,
        " because of the observed condition-specific temporal support."
      )
    )
  }

  structure(
    list(
      version = "0.5.0.9000",
      prepared = prepared,
      temporal_structure = temporal_structure,
      family = family,

      # Existing field remains the effective value consumed by the
      # translator, preserving downstream compatibility.
      smooth_basis_dimension = effective_k,

      smooth_basis_dimension_requested = requested_k,
      smooth_basis_dimension_effective = effective_k,
      smooth_basis_support = support_k,
      smooth_basis_adjusted = basis_adjusted,

      gp_spec = if (temporal_structure == "gaussian_process") {
        gp_spec
      } else {
        NULL
      },
      residual_correlation = residual_correlation,
      item_effects = item_effects,
      prior_scales = prior_scales,
      fit_performed = FALSE,
      governance = governance_text
    ),
    class = "gp3bayes_binocular_pupil_specification"
  )
}

#' Translate a binocular specification to a brms multivariate formula
#'
#' @param specification A binocular specification.
#' @return A `gp3bayes_binocular_brms_specification` object.
#' @export
translate_binocular_pupil_model_to_brms <- function(specification) {
  if (!inherits(specification, "gp3bayes_binocular_pupil_specification")) stop("Expected a binocular specification.", call. = FALSE)
  .p05_require("brms", "to translate binocular models")
  p <- specification$prepared
  d <- p$data
  left_f <- .p05_binocular_rhs(specification, p$mapping$left)
  right_f <- .p05_binocular_rhs(specification, p$mapping$right)
  formula <- brms::bf(left_f) + brms::bf(right_f) + brms::set_rescor(specification$residual_correlation)
  family <- list(brms::brmsfamily(specification$family), brms::brmsfamily(specification$family))

  y <- c(d[[p$mapping$left]], d[[p$mapping$right]])
  y_sd <- .p05_safe_sd(y)
  y_med <- stats::median(y, na.rm = TRUE)
  ov <- specification$prior_scales
  scl <- function(nm, default) {
    if (!is.null(ov) && nm %in% names(ov)) unname(ov[[nm]]) else default
  }

  # Match candidate priors against the parameters actually generated by the
  # multivariate formula. This keeps linear, smooth, GP, Gaussian, Student-t,
  # and residual-correlation variants from receiving irrelevant prior classes.
  available <- brms::get_prior(
    formula = formula,
    data = d,
    family = family
  )

  priors <- NULL

  add_prior <- function(current, prior) {
    if (is.null(current)) prior else c(current, prior)
  }

  response_labels <- if ("resp" %in% names(available)) {
    unique(
      available$resp[
        !is.na(available$resp) & nzchar(available$resp)
      ]
    )
  } else {
    character()
  }

  normalize_response <- function(x) {
    tolower(gsub("[^[:alnum:]]", "", x))
  }

  response_columns <- c(
    p$mapping$left,
    p$mapping$right
  )

  resolve_response_column <- function(resp) {
    hit <- response_columns[
      normalize_response(response_columns) ==
        normalize_response(resp)
    ]

    if (length(hit) != 1L) {
      stop(
        "Could not map multivariate brms response `",
        resp,
        "` back to exactly one binocular response column.",
        call. = FALSE
      )
    }

    hit[[1L]]
  }

  for (resp in response_labels) {
    response_column <- resolve_response_column(resp)
    z <- d[[response_column]]
    response_sd <- .p05_safe_sd(z)
    response_median <- stats::median(z, na.rm = TRUE)

    rows <- available[
      !is.na(available$resp) &
        available$resp == resp,
      ,
      drop = FALSE
    ]

    classes <- unique(rows$class)

    if ("Intercept" %in% classes) {
      priors <- add_prior(
        priors,
        brms::set_prior(
          sprintf(
            "normal(%.12g, %.12g)",
            response_median,
            scl("intercept", 2 * response_sd)
          ),
          class = "Intercept",
          resp = resp
        )
      )
    }

    if ("b" %in% classes) {
      priors <- add_prior(
        priors,
        brms::set_prior(
          sprintf(
            "normal(0, %.12g)",
            scl("b", response_sd)
          ),
          class = "b",
          resp = resp
        )
      )
    }

    for (cls in intersect(
      c("sd", "sds", "sdgp"),
      classes
    )) {
      priors <- add_prior(
        priors,
        brms::set_prior(
          sprintf(
            "student_t(3, 0, %.12g)",
            scl(cls, response_sd)
          ),
          class = cls,
          resp = resp
        )
      )
    }

    if ("sigma" %in% classes) {
      priors <- add_prior(
        priors,
        brms::set_prior(
          sprintf(
            "student_t(3, 0, %.12g)",
            scl("sigma", response_sd)
          ),
          class = "sigma",
          resp = resp
        )
      )
    }

    if ("nu" %in% classes) {
      priors <- add_prior(
        priors,
        brms::set_prior(
          sprintf(
            "gamma(2, %.12g)",
            scl("nu_rate", 0.1)
          ),
          class = "nu",
          resp = resp
        )
      )
    }
  }

  if (any(available$class == "rescor")) {
    priors <- add_prior(
      priors,
      brms::set_prior(
        sprintf(
          "lkj(%.12g)",
          scl("rescor_eta", 2)
        ),
        class = "rescor"
      )
    )
  }

  structure(
    list(specification = specification, formula = formula, family = family, priors = priors, data = d),
    class = "gp3bayes_binocular_brms_specification"
  )
}

#' Fit a joint binocular pupil model
#'
#' @param specification A binocular specification.
#' @param backend rstan or cmdstanr.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted sampling controls.
#' @export
fit_binocular_pupil_model <- function(
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
  tr <- translate_binocular_pupil_model_to_brms(specification)
  fit <- brms::brm(
    formula = tr$formula, data = tr$data, family = tr$family, prior = tr$priors,
    backend = backend, chains = chains, iter = iter, warmup = warmup, cores = cores,
    seed = seed, control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    refresh = refresh, save_pars = brms::save_pars(all = TRUE)
  )
  structure(
    list(specification = specification, translation = tr, backend_fit = fit, backend = backend, fit_performed = TRUE),
    class = "gp3bayes_binocular_pupil_fit"
  )
}

.p05_binocular_grid <- function(fit, newdata = NULL, max_grid = 5000L) {
  if (!inherits(fit, "gp3bayes_binocular_pupil_fit")) stop("Expected a binocular fit.", call. = FALSE)
  max_grid <- .p05_assert_integerish(max_grid, "max_grid", 1L, 1000000L)
  p <- fit$specification$prepared
  d <- p$data
  m <- p$mapping
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) stop("`newdata` must be a data.frame.", call. = FALSE)
    if (nrow(newdata) > max_grid) stop("`newdata` exceeds `max_grid`.", call. = FALSE)
    return(newdata)
  }
  times <- sort(unique(d[[m$time]]))
  if (length(times) > 200L) times <- unique(as.numeric(stats::quantile(times, seq(0, 1, length.out = 200), names = FALSE)))
  cond <- unique(d[[m$condition]])
  cond <- cond[!is.na(cond)]
  grid <- expand.grid(.time = times, .condition = cond, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(grid) <- c(m$time, m$condition)
  if (is.factor(d[[m$condition]])) grid[[m$condition]] <- factor(grid[[m$condition]], levels = levels(d[[m$condition]]))
  grid[[m$participant]] <- d[[m$participant]][which(!is.na(d[[m$participant]]))[[1L]]]
  if (!is.null(m$item) && fit$specification$item_effects) grid[[m$item]] <- d[[m$item]][which(!is.na(d[[m$item]]))[[1L]]]
  for (v in p$covariates) {
    x <- d[[v]]
    grid[[v]] <- if (is.numeric(x)) stats::median(x, na.rm = TRUE) else x[which(!is.na(x))[[1L]]]
  }
  if (nrow(grid) > max_grid) stop("Prediction grid exceeds `max_grid`.", call. = FALSE)
  grid
}

#' Estimate joint binocular posterior trajectories
#'
#' @param fit A binocular fit.
#' @param newdata Optional prediction grid.
#' @param ndraws Posterior draws.
#' @param probability Central interval probability.
#' @return A `gp3bayes_binocular_pupil_trajectory` object.
#' @export
estimate_binocular_pupil_trajectory <- function(fit, newdata = NULL, ndraws = 500L, probability = 0.95) {
  .p05_assert_probability(probability, "probability")
  ndraws <- .p05_assert_integerish(ndraws, "ndraws", 1L, 100000L)
  grid <- .p05_binocular_grid(fit, newdata)
  bfit <- .p05_underlying_fit(fit)
  m <- fit$specification$prepared$mapping
  left <- brms::posterior_epred(bfit, newdata = grid, resp = m$left, ndraws = ndraws, re_formula = NA)
  right <- brms::posterior_epred(bfit, newdata = grid, resp = m$right, ndraws = ndraws, re_formula = NA)
  structure(
    list(grid = grid, left_draws = left, right_draws = right, probability = probability, mapping = m),
    class = "gp3bayes_binocular_pupil_trajectory"
  )
}

#' Estimate posterior right-minus-left binocular differences
#'
#' @param x A binocular trajectory object.
#' @return A data frame.
#' @export
pupil_binocular_difference <- function(x) {
  if (!inherits(x, "gp3bayes_binocular_pupil_trajectory")) stop("Expected a binocular trajectory.", call. = FALSE)
  d <- x$right_draws - x$left_draws
  alpha <- (1 - x$probability) / 2
  cbind(x$grid, .p05_quantile_summary(d, c(alpha, 0.5, 1 - alpha)), row.names = NULL)
}

#' Extract posterior residual binocular correlation
#'
#' @param fit A binocular fit with residual correlation enabled.
#' @param probability Central interval probability.
#' @return A data frame.
#' @export
pupil_binocular_correlation <- function(fit, probability = 0.95) {
  if (!inherits(fit, "gp3bayes_binocular_pupil_fit")) stop("Expected a binocular fit.", call. = FALSE)
  if (!fit$specification$residual_correlation) stop("Residual correlation was disabled in this specification.", call. = FALSE)
  .p05_require("posterior", "to extract residual correlation")
  draws <- posterior::as_draws_df(.p05_underlying_fit(fit))
vars <- names(draws)[startsWith(names(draws), "rescor__")]
  if (!length(vars)) stop("No residual-correlation parameter was found.", call. = FALSE)
  alpha <- (1 - probability) / 2
  s <- .p05_quantile_summary(posterior::as_draws_matrix(posterior::subset_draws(draws, variable = vars)), c(alpha, 0.5, 1 - alpha))
  s$parameter <- vars
  s[, c("parameter", "mean", "sd", "q_low", "median", "q_high")]
}

#' Summarise binocular posterior agreement
#'
#' @param trajectory A binocular trajectory object.
#' @param tolerance A scientifically declared absolute right-minus-left tolerance.
#' @return A data frame with posterior agreement probabilities.
#' @export
pupil_binocular_agreement_table <- function(trajectory, tolerance = 0.1) {
  if (!inherits(trajectory, "gp3bayes_binocular_pupil_trajectory")) stop("Expected a binocular trajectory.", call. = FALSE)
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0) stop("`tolerance` must be positive and finite.", call. = FALSE)
  d <- trajectory$right_draws - trajectory$left_draws
  base <- pupil_binocular_difference(trajectory)
  base$probability_within_tolerance <- colMeans(abs(d) <= tolerance)
  base$tolerance <- tolerance
  base
}

#' @export
print.gp3bayes_binocular_pupil_prepared <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_prepared>\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Left:", x$mapping$left, "\n")
  cat("  Right:", x$mapping$right, "\n")
  invisible(x)
}

#' @export
print.gp3bayes_binocular_pupil_audit <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_audit>\n")
  cat("  Status:", x$status, "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_binocular_pupil_audit <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_binocular_pupil_specification <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_specification>\n")
  cat("  Family:", x$family, "\n")
  cat("  Temporal structure:", x$temporal_structure, "\n")
  cat("  Residual correlation:", x$residual_correlation, "\n")
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_binocular_pupil_fit <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_fit>\n")
  cat("  Backend:", x$backend, "\n")
  cat("  Family:", x$specification$family, "\n")
  cat("  Fit performed: TRUE\n")
  invisible(x)
}

#' @export
print.gp3bayes_binocular_pupil_trajectory <- function(x, ...) {
  cat("<gp3bayes_binocular_pupil_trajectory>\n")
  cat("  Grid rows:", nrow(x$grid), "\n")
  cat("  Draws:", nrow(x$left_draws), "\n")
  invisible(x)
}

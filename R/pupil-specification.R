.gp3p_prior_defaults <- function(prepared, prior_scales = NULL) {
  unit <- prepared$model_unit
  d <- prepared$data$.pupil_model
  center <- stats::median(d, na.rm = TRUE)
  if (!is.finite(center)) .gp3p_stop("Cannot construct priors from an all-missing pupil outcome.")

  if (unit %in% c("pixels", "arbitrary_units")) {
    if (is.null(prior_scales)) {
      .gp3p_stop(
        "`prior_scales` is required for pixel or arbitrary-unit pupil outcomes. ",
        "Supply named positive values: intercept, coefficient, group_sd, residual, smooth_sd."
      )
    }
  }
  defaults <- switch(
    unit,
    millimetres = c(intercept = 2, coefficient = 1, group_sd = 1,
                    residual = 1, smooth_sd = 1, ar = 0.3),
    metres = c(intercept = 0.002, coefficient = 0.001, group_sd = 0.001,
               residual = 0.001, smooth_sd = 0.001, ar = 0.3),
    standardized = c(intercept = 1, coefficient = 0.5, group_sd = 0.5,
                     residual = 1, smooth_sd = 0.5, ar = 0.3),
    proportion_change = c(intercept = 0.2, coefficient = 0.1, group_sd = 0.1,
                          residual = 0.2, smooth_sd = 0.1, ar = 0.3),
    percent_change = c(intercept = 20, coefficient = 10, group_sd = 10,
                       residual = 20, smooth_sd = 10, ar = 0.3),
    c(intercept = NA_real_, coefficient = NA_real_, group_sd = NA_real_,
      residual = NA_real_, smooth_sd = NA_real_, ar = 0.3)
  )
  if (!is.null(prior_scales)) {
    if (!is.numeric(prior_scales) || is.null(names(prior_scales))) {
      .gp3p_stop("`prior_scales` must be a named numeric vector or list.")
    }
    prior_scales <- unlist(prior_scales)
    required <- c("intercept", "coefficient", "group_sd", "residual", "smooth_sd")
    missing <- setdiff(required, names(prior_scales))
    if (length(missing)) {
      .gp3p_stop("`prior_scales` is missing: ", paste(missing, collapse = ", "), ".")
    }
    if (any(!is.finite(prior_scales[required])) || any(prior_scales[required] <= 0)) {
      .gp3p_stop("All required `prior_scales` values must be finite and positive.")
    }
    defaults[names(prior_scales)] <- prior_scales
  }
  list(center = center, scales = defaults)
}

.gp3p_formula_name <- function(x) {
  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", x)) x else paste0("`", gsub("`", "\\`", x), "`")
}

.gp3p_build_pupil_formula <- function(
    temporal_structure, smooth_k, condition_trajectory,
    participant_trajectory, item_effects, covariates, autocorrelation,
    has_condition, has_item) {

  terms <- character()
  if (has_condition) terms <- c(terms, ".condition")

  if (identical(temporal_structure, "smooth")) {
    if (condition_trajectory && has_condition) {
      terms <- c(terms, paste0("s(.event_time, by = .condition, k = ", smooth_k, ")"))
    } else {
      terms <- c(terms, paste0("s(.event_time, k = ", smooth_k, ")"))
    }
  } else {
    if (condition_trajectory && has_condition) {
      terms <- c(terms, ".event_time", ".event_time:.condition")
    } else {
      terms <- c(terms, ".event_time")
    }
  }

  terms <- c(terms, vapply(covariates, .gp3p_formula_name, character(1)))
  terms <- c(terms, "(1 | .participant)")
  if (has_item && item_effects) terms <- c(terms, "(1 | .item)")
  if (identical(participant_trajectory, "factor_smooth")) {
    k_fs <- max(3L, min(as.integer(smooth_k), 6L))
    terms <- c(terms,
               paste0("s(.event_time, .participant, bs = \"fs\", m = 1, k = ", k_fs, ")"))
  }
  if (identical(autocorrelation, "ar1")) {
    terms <- c(terms, "ar(time = .sample_index, gr = .series_id, p = 1)")
  }

  stats::as.formula(
    paste(".pupil_model ~", paste(terms, collapse = " + ")),
    env = baseenv()
  )
}

#' Specify the restricted hierarchical pupil time-course model
#'
#' Constructs an inspectable, closed-set Gaussian model specification. Users
#' cannot supply a raw formula or arbitrary family.
#'
#' @param prepared A `prepare_pupil_timecourse()` result.
#' @param temporal_structure `"smooth"` or `"linear"`.
#' @param smooth_basis_dimension Basis dimension for approved smooth terms.
#' @param condition_trajectory `NULL` (default) uses a separate trajectory
#'   when condition is declared; otherwise supply `TRUE` or `FALSE` explicitly.
#' @param autocorrelation `"ar1"` or `"none"`. AR(1) is blocked when the
#'   observed sampling-interval coefficient of variation exceeds the recorded
#'   readiness threshold because sample-order AR(1) is not a continuous-time
#'   irregular-sampling model.
#' @param participant_trajectory `"none"` or the restricted factor-smooth
#'   option `"factor_smooth"`.
#' @param item_effects `NULL` (default) includes an item random intercept only
#'   when at least two item levels are declared; otherwise supply `TRUE` or
#'   `FALSE` explicitly.
#' @param covariates Character vector of already-declared numeric nuisance
#'   covariates in the prepared data.
#' @param prior_scales Optional named positive scale values. Required for
#'   pixels and arbitrary units.
#' @return A `gp3bayes_pupil_model_specification`.
#' @section Priors:
#' Defaults are unit-aware weak regularizers for physical millimetres/metres
#' and declared transformed scales. Pixel and arbitrary-unit outcomes require
#' user-declared prior scales because tracker-specific units are not
#' interchangeable.
#' @section Governance boundary:
#' No unrestricted formula, likelihood family, smooth, autocorrelation order,
#' or backend argument is accepted.
#' @examples
#' sim <- simulate_pupil_timecourse(
#'   n_participants = 3, trials_per_participant = 3,
#'   sampling_frequency = 20, seed = 2
#' )
#' contract <- create_pupil_contract(
#'   "pupil_mm", "participant_id", "trial_id", "event_time",
#'   "millimetres", 20, condition_col = "condition"
#' )
#' prepared <- prepare_pupil_timecourse(sim$data, contract)
#' specify_pupil_timecourse_model(prepared, autocorrelation = "none")
#' @export
specify_pupil_timecourse_model <- function(
    prepared,
    temporal_structure = c("smooth", "linear"),
    smooth_basis_dimension = 10L,
    condition_trajectory = NULL,
    autocorrelation = c("ar1", "none"),
    participant_trajectory = c("none", "factor_smooth"),
    item_effects = NULL,
    covariates = character(),
    prior_scales = NULL) {

  if (!inherits(prepared, "gp3bayes_pupil_prepared")) {
    .gp3p_stop("`prepared` must be created by `prepare_pupil_timecourse()`.")
  }
  temporal_structure <- match.arg(temporal_structure)
  autocorrelation <- match.arg(autocorrelation)
  participant_trajectory <- match.arg(participant_trajectory)
  smooth_basis_dimension <- .gp3p_positive(
    smooth_basis_dimension, "smooth_basis_dimension", TRUE
  )
  if (smooth_basis_dimension < 3L) {
    .gp3p_stop("`smooth_basis_dimension` must be at least 3.")
  }
  n_participants <- nlevels(prepared$data$.participant)
  if (n_participants < 2L) {
    .gp3p_stop(
      "The approved hierarchical pupil family requires at least two participants. ",
      "A one-participant series can be audited and plotted but not fitted under this contract."
    )
  }
  declared_condition <- ".condition" %in% names(prepared$data)
  n_conditions <- if (declared_condition) nlevels(prepared$data$.condition) else 0L
  has_condition <- declared_condition && n_conditions >= 2L
  if (is.null(condition_trajectory)) {
    condition_trajectory <- has_condition
  } else {
    condition_trajectory <- .gp3p_flag(condition_trajectory, "condition_trajectory")
  }
  if (condition_trajectory && !has_condition) {
    .gp3p_stop(
      "`condition_trajectory = TRUE` requires a declared condition with at least two levels."
    )
  }

  declared_item <- ".item" %in% names(prepared$data)
  n_items <- if (declared_item) nlevels(prepared$data$.item) else 0L
  if (is.null(item_effects)) {
    item_effects <- declared_item && n_items >= 2L
  } else {
    item_effects <- .gp3p_flag(item_effects, "item_effects")
    if (item_effects && (!declared_item || n_items < 2L)) {
      .gp3p_stop("`item_effects = TRUE` requires a declared item with at least two levels.")
    }
  }

  covariates <- .gp3p_character_vector(covariates, "covariates")
  .gp3p_check_columns(prepared$data, covariates, "covariate")
  if (identical(participant_trajectory, "factor_smooth") &&
      !identical(temporal_structure, "smooth")) {
    .gp3p_stop("Participant factor-smooth variation requires `temporal_structure = \"smooth\"`.")
  }
  if (identical(temporal_structure, "smooth")) {
    n_time <- length(unique(prepared$data$.event_time))
    if (smooth_basis_dimension > n_time) {
      .gp3p_stop(
        "`smooth_basis_dimension` exceeds the number of unique event-time values (",
        n_time, ")."
      )
    }
    if (has_condition) {
      time_by_condition <- vapply(
        split(prepared$data$.event_time, prepared$data$.condition),
        function(z) length(unique(z)),
        integer(1)
      )
      if (any(time_by_condition < smooth_basis_dimension)) {
        .gp3p_stop(
          "Each condition requires at least `smooth_basis_dimension` unique event-time values."
        )
      }
    }
  }
  if (identical(autocorrelation, "ar1")) {
    cv <- prepared$timing$cv_dt
    cutoff <- prepared$timing$irregularity_review_cv
    if (!is.finite(cv) || cv > cutoff) {
      .gp3p_stop(
        "AR(1) specification blocked because sampling intervals are too irregular ",
        "for the approved sample-order AR(1) contract (CV=", signif(cv, 4),
        "; threshold=", cutoff, "). Use `autocorrelation = \"none\"` or address ",
        "sampling upstream."
      )
    }
  }

  for (nm in covariates) {
    if (!is.numeric(prepared$data[[nm]])) {
      .gp3p_stop("Declared pupil-model covariate `", nm, "` must be numeric.")
    }
  }

  priors <- .gp3p_prior_defaults(prepared, prior_scales)
  formula <- .gp3p_build_pupil_formula(
    temporal_structure = temporal_structure,
    smooth_k = smooth_basis_dimension,
    condition_trajectory = condition_trajectory,
    participant_trajectory = participant_trajectory,
    item_effects = item_effects,
    covariates = covariates,
    autocorrelation = autocorrelation,
    has_condition = has_condition,
    has_item = ".item" %in% names(prepared$data)
  )

  prior_table <- data.frame(
    class = c("Intercept", "b", "sd", "sigma",
              if (identical(temporal_structure, "smooth")) "sds" else character(),
              if (identical(autocorrelation, "ar1")) "ar" else character()),
    distribution = c(
      paste0("normal(", signif(priors$center, 6), ", ", priors$scales["intercept"], ")"),
      paste0("normal(0, ", priors$scales["coefficient"], ")"),
      paste0("student_t(3, 0, ", priors$scales["group_sd"], ")"),
      paste0("student_t(3, 0, ", priors$scales["residual"], ")"),
      if (identical(temporal_structure, "smooth"))
        paste0("student_t(3, 0, ", priors$scales["smooth_sd"], ")") else character(),
      if (identical(autocorrelation, "ar1"))
        paste0("normal(0, ", priors$scales["ar"], ")") else character()
    ),
    unit = prepared$model_unit,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      specification_version = "0.4-pupil-1",
      family = "pupil",
      model_family = "Gaussian",
      likelihood = "Gaussian",
      link = "identity",
      contract = prepared$contract,
      prepared = prepared,
      formula = formula,
      formula_text = paste(deparse(formula), collapse = " "),
      priors = prior_table,
      prior_center = priors$center,
      prior_scales = priors$scales,
      temporal_structure = temporal_structure,
      smooth_basis_dimension = smooth_basis_dimension,
      condition_trajectory = condition_trajectory,
      condition_declared = declared_condition,
      condition_levels = n_conditions,
      autocorrelation = autocorrelation,
      participant_effects = "random_intercept",
      participant_trajectory = participant_trajectory,
      item_effects = item_effects,
      item_declared = declared_item,
      item_levels = n_items,
      covariates = covariates,
      outcome_unit = prepared$model_unit,
      baseline_status = list(
        operation = prepared$baseline_operation,
        window = prepared$baseline_window,
        upstream_applied = prepared$contract$preprocessing$baseline_applied
      ),
      preprocessing_provenance = prepared$contract$preprocessing,
      unrestricted_formula = FALSE,
      unrestricted_family = FALSE,
      fitting_engine = "brms",
      approved_backends = c("rstan", "cmdstanr"),
      fit_performed = FALSE
    ),
    class = c(
      "gp3bayes_pupil_model_specification",
      "gp3bayes_model_specification"
    )
  )
}

#' Convert a pupil specification to a publication-ready table
#' @param x A pupil model specification.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_specification_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_model_specification")) {
    .gp3p_stop("`x` must be a pupil model specification.")
  }
  data.frame(
    field = c(
      "family", "likelihood", "link", "formula", "temporal_structure",
      "smooth_basis_dimension", "condition_trajectory", "autocorrelation",
      "participant_effects", "participant_trajectory", "item_effects",
      "covariates", "outcome_unit", "baseline_operation", "unrestricted_formula"
    ),
    value = c(
      x$family, x$likelihood, x$link, x$formula_text, x$temporal_structure,
      x$smooth_basis_dimension, x$condition_trajectory, x$autocorrelation,
      x$participant_effects, x$participant_trajectory, x$item_effects,
      paste(x$covariates, collapse = ", "), x$outcome_unit,
      x$baseline_status$operation, x$unrestricted_formula
    ),
    stringsAsFactors = FALSE
  )
}

#' Translate an approved pupil model to brms
#'
#' Creates a fixed Gaussian `brms` representation without compiling or fitting.
#' @param specification A pupil model specification.
#' @return A `gp3bayes_pupil_brms_translation`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
translate_pupil_model_to_brms <- function(specification) {
  if (!inherits(specification, "gp3bayes_pupil_model_specification")) {
    .gp3p_stop("`specification` must be created by `specify_pupil_timecourse_model()`.")
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    .gp3p_stop("Package `brms` is required for backend translation.")
  }

  priors <- lapply(seq_len(nrow(specification$priors)), function(i) {
    row <- specification$priors[i, ]
    brms::set_prior(row$distribution, class = row$class)
  })
  prior <- do.call(c, priors)

  structure(
    list(
      translation_version = "0.4-pupil-1",
      family = "pupil",
      formula = specification$formula,
      data = specification$prepared$data[!is.na(specification$prepared$data$.pupil_model), ,
                                         drop = FALSE],
      brms_family = brms::brmsfamily("gaussian", link = "identity"),
      prior = prior,
      specification = specification,
      unrestricted_formula = FALSE,
      unrestricted_family = FALSE,
      fit_performed = FALSE
    ),
    class = "gp3bayes_pupil_brms_translation"
  )
}


#' Check the approved pupil priors predictively
#'
#' Creates a governed prior-predictive plan by default. With
#' `execute = TRUE`, draws from the prior-only approved Gaussian `brms` model
#' and compares replicated pupil values with the observed model-scale range.
#' The check reports evidence only and never changes priors automatically.
#'
#' @param specification Approved pupil model specification.
#' @param execute Whether to run prior-only MCMC. Defaults to `FALSE`.
#' @param backend Approved backend, `"rstan"` or `"cmdstanr"`.
#' @param draws Number of prior predictive replicated draws to retain.
#' @param chains,iter,warmup,cores,seed Sampling controls. Package-controlled
#'   cores are capped at two.
#' @param probability Central predictive interval probability.
#' @param max_cells Maximum retained draw-by-observation cells.
#' @return A `gp3bayes_pupil_prior_predictive` evidence object.
#' @section Governance boundary:
#' This operation does not tune priors, select a favourable prior scale, or
#' certify a model as scientifically adequate. `execute = FALSE` performs no
#' compilation or fitting.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
check_pupil_prior_predictive <- function(
    specification,
    execute = FALSE,
    backend = c("rstan", "cmdstanr"),
    draws = 200L,
    chains = 2L,
    iter = 1000L,
    warmup = 500L,
    cores = min(2L, chains),
    seed = 2026,
    probability = 0.95,
    max_cells = 3000000L) {
  if (!inherits(specification, "gp3bayes_pupil_model_specification")) {
    .gp3p_stop("`specification` must be an approved pupil model specification.")
  }
  execute <- .gp3p_flag(execute, "execute")
  backend <- match.arg(backend)
  draws <- .gp3p_positive(draws, "draws", TRUE)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  s <- .gp3p_validate_sampling(
    chains, iter, warmup, cores, 0.95, 12L, 0L
  )
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) .gp3p_stop("`seed` must be one integer.")

  plan <- data.frame(
    field = c("family", "backend", "draws", "chains", "iter", "warmup",
              "cores", "outcome_unit", "execute"),
    value = as.character(c(
      "Gaussian", backend, draws, s$chains, s$iter, s$warmup, s$cores,
      specification$outcome_unit, execute
    )),
    stringsAsFactors = FALSE
  )

  if (!execute) {
    return(structure(
      list(
        status = "planned",
        plan = plan,
        summary = data.frame(),
        backend_fit = NULL,
        executed = FALSE,
        priors_changed = FALSE,
        adequacy_certified = FALSE
      ),
      class = "gp3bayes_pupil_prior_predictive"
    ))
  }

  translation <- translate_pupil_model_to_brms(specification)
  n_obs <- nrow(translation$data)
  if (n_obs * draws > max_cells) {
    .gp3p_stop("Requested prior predictive expansion exceeds `max_cells`.")
  }
  if (identical(backend, "cmdstanr")) {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      .gp3p_stop("Package `cmdstanr` is required for the cmdstanr backend.")
    }
    path <- try(cmdstanr::cmdstan_path(), silent = TRUE)
    if (inherits(path, "try-error") || !nzchar(path)) {
      .gp3p_stop("A working CmdStan installation is required for cmdstanr prior prediction.")
    }
  } else if (!requireNamespace("rstan", quietly = TRUE)) {
    .gp3p_stop("Package `rstan` is required for the rstan backend.")
  }

  prior_fit <- brms::brm(
    formula = translation$formula,
    data = translation$data,
    family = translation$brms_family,
    prior = translation$prior,
    backend = backend,
    algorithm = "sampling",
    sample_prior = "only",
    chains = s$chains,
    iter = s$iter,
    warmup = s$warmup,
    cores = s$cores,
    seed = seed,
    refresh = 0L,
    control = list(adapt_delta = 0.95, max_treedepth = 12L)
  )
  yrep <- brms::posterior_predict(prior_fit, ndraws = draws)
  if (!is.matrix(yrep)) yrep <- matrix(yrep, nrow = draws)
  alpha <- (1 - probability) / 2
  row_range <- t(apply(yrep, 1L, range, na.rm = TRUE))
  obs <- translation$data$.pupil_model
  summary <- data.frame(
    statistic = c("mean", "sd", "minimum", "maximum"),
    observed = c(mean(obs), stats::sd(obs), min(obs), max(obs)),
    prior_predictive_median = c(
      stats::median(rowMeans(yrep)),
      stats::median(apply(yrep, 1L, stats::sd)),
      stats::median(row_range[, 1L]),
      stats::median(row_range[, 2L])
    ),
    lower = c(
      stats::quantile(rowMeans(yrep), alpha, names = FALSE, type = 8),
      stats::quantile(apply(yrep, 1L, stats::sd), alpha, names = FALSE, type = 8),
      stats::quantile(row_range[, 1L], alpha, names = FALSE, type = 8),
      stats::quantile(row_range[, 2L], alpha, names = FALSE, type = 8)
    ),
    upper = c(
      stats::quantile(rowMeans(yrep), 1 - alpha, names = FALSE, type = 8),
      stats::quantile(apply(yrep, 1L, stats::sd), 1 - alpha, names = FALSE, type = 8),
      stats::quantile(row_range[, 1L], 1 - alpha, names = FALSE, type = 8),
      stats::quantile(row_range[, 2L], 1 - alpha, names = FALSE, type = 8)
    ),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      status = "evidence",
      plan = plan,
      summary = summary,
      backend_fit = prior_fit,
      executed = TRUE,
      priors_changed = FALSE,
      adequacy_certified = FALSE
    ),
    class = "gp3bayes_pupil_prior_predictive"
  )
}

#' @export
print.gp3bayes_pupil_prior_predictive <- function(x, ...) {
  cat("<gp3bayes_pupil_prior_predictive>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Executed: ", x$executed, "\n", sep = "")
  cat("  Priors changed automatically: FALSE\n")
  cat("  Adequacy certified: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_prior_predictive <- function(x, ...) {
  if (isTRUE(x$executed)) x$summary else x$plan
}

#' @export
print.gp3bayes_pupil_model_specification <- function(x, ...) {
  cat("<gp3bayes_pupil_model_specification>\n")
  cat("  Family: Gaussian pupil time-course\n")
  cat("  Formula: ", x$formula_text, "\n", sep = "")
  cat("  Temporal structure: ", x$temporal_structure, "\n", sep = "")
  cat("  Condition trajectory: ", x$condition_trajectory, "\n", sep = "")
  cat("  Autocorrelation: ", x$autocorrelation, "\n", sep = "")
  cat("  Outcome unit: ", x$outcome_unit, "\n", sep = "")
  cat("  Baseline: ", x$baseline_status$operation, "\n", sep = "")
  cat("  Unrestricted formula: FALSE\n")
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_model_specification <- function(x, ...) {
  pupil_specification_table(x)
}

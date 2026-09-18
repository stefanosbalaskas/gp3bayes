# Bayesian multilevel mediation for trial-level gaze data ----------------------
# Consumes canonical preparation from eyeprocess. Does not decompose variables.

.gp3b_med_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3b_med_family <- function(family, role = c("mediator", "outcome")) {
  role <- match.arg(role)
  aliases <- c(
    normal = "gaussian", binary = "bernoulli", binomial = "bernoulli",
    count = "poisson", negbin = "negative_binomial",
    negativebinomial = "negative_binomial", ordered = "ordinal"
  )
  key <- tolower(gsub("[- ]", "_", family))
  if (key %in% names(aliases)) key <- unname(aliases[[key]])
  allowed <- if (role == "mediator") {
    c("gaussian", "lognormal", "gamma", "beta", "bernoulli", "poisson", "negative_binomial")
  } else {
    c("gaussian", "bernoulli", "poisson", "negative_binomial", "ordinal")
  }
  if (!key %in% allowed) {
    .gp3b_med_stop(
      "Unsupported `", role, "_family` `", family, "`. Supported values: ",
      paste(allowed, collapse = ", "), "."
    )
  }
  key
}

.gp3b_med_positive <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
    .gp3b_med_stop("`", name, "` must be one finite positive number.")
  }
  as.numeric(value)
}

#' Create Priors for Multilevel Gaze Mediation
#'
#' @export
create_mediation_prior_specification <- function(
  intercept_sd = 2.5,
  coefficient_sd = 1,
  group_sd_scale = 1,
  residual_sd_scale = 1,
  dispersion_rate = 1,
  beta_precision_rate = 0.1
) {
  structure(
    list(
      intercept_sd = .gp3b_med_positive(intercept_sd, "intercept_sd"),
      coefficient_sd = .gp3b_med_positive(coefficient_sd, "coefficient_sd"),
      group_sd_scale = .gp3b_med_positive(group_sd_scale, "group_sd_scale"),
      residual_sd_scale = .gp3b_med_positive(residual_sd_scale, "residual_sd_scale"),
      dispersion_rate = .gp3b_med_positive(dispersion_rate, "dispersion_rate"),
      beta_precision_rate = .gp3b_med_positive(beta_precision_rate, "beta_precision_rate")
    ),
    class = "gp3bayes_mediation_prior_specification"
  )
}

.gp3b_med_prepared <- function(prepared) {
  if (!inherits(prepared, "eye_multilevel_mediation_data")) {
    .gp3b_med_stop(
      "`prepared` must inherit from `eye_multilevel_mediation_data`; prepare data in eyeprocess first."
    )
  }
  required <- c("X_within", "X_between", "M_within", "M_between", "mediation_analysis_eligible")
  missing <- setdiff(required, names(prepared$data))
  if (length(missing)) {
    .gp3b_med_stop("Prepared data is missing canonical columns: ", paste(missing, collapse = ", "), ".")
  }
  prepared
}

.gp3b_med_select_rows <- function(data, required, missingness_policy) {
  choices <- c("error", "complete_case", "quality_eligible")
  if (!missingness_policy %in% choices) {
    .gp3b_med_stop("`missingness_policy` must be one of: ", paste(choices, collapse = ", "), ".")
  }
  complete <- stats::complete.cases(data[required])
  eligible <- as.logical(data$mediation_analysis_eligible)
  eligible[is.na(eligible)] <- FALSE
  if (missingness_policy == "error") {
    keep <- complete & eligible
    if (!all(keep)) {
      .gp3b_med_stop(
        sum(!keep), " trial row(s) are incomplete or quality-ineligible. Choose an explicit ",
        "`missingness_policy` after reviewing preparation audits."
      )
    }
    keep <- rep(TRUE, nrow(data))
  } else if (missingness_policy == "complete_case") {
    keep <- complete
  } else {
    keep <- complete & eligible
  }
  list(
    data = data[keep, , drop = FALSE],
    excluded = which(!keep)
  )
}

.gp3b_med_validate_family_values <- function(x, family, role) {
  if (!is.numeric(x)) x <- suppressWarnings(as.numeric(as.character(x)))
  if (anyNA(x)) .gp3b_med_stop("`", role, "` contains missing values after row selection.")
  if (family %in% c("lognormal", "gamma") && any(x <= 0)) {
    .gp3b_med_stop("Family `", family, "` requires strictly positive ", role, " values.")
  }
  if (family == "beta" && any(x <= 0 | x >= 1)) {
    .gp3b_med_stop("Family `beta` requires ", role, " values strictly between 0 and 1.")
  }
  if (family == "bernoulli" && any(!x %in% c(0, 1))) {
    .gp3b_med_stop("Family `bernoulli` requires ", role, " values coded 0/1.")
  }
  if (family %in% c("poisson", "negative_binomial") && any(x < 0 | x != floor(x))) {
    .gp3b_med_stop("Family `", family, "` requires non-negative integer ", role, " counts.")
  }
  if (family == "ordinal") {
    values <- sort(unique(x))
    if (length(values) < 3L || !identical(values, seq(min(values), max(values)))) {
      .gp3b_med_stop("Ordinal outcomes must be contiguous integer categories with at least three levels.")
    }
  }
  invisible(TRUE)
}

.gp3b_med_has_variation <- function(x, tolerance = 1e-12) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  length(x) >= 2L && is.finite(stats::var(x)) && stats::var(x) > tolerance
}

.gp3b_med_estimable_simple_paths <- function(data) {
  paths <- character()
  if (.gp3b_med_has_variation(data$X_within)) paths <- c(paths, "a_within", "cprime_within")
  if (.gp3b_med_has_variation(data$M_within)) paths <- c(paths, "b_within")
  if (.gp3b_med_has_variation(data$X_between)) paths <- c(paths, "a_between", "cprime_between")
  if (.gp3b_med_has_variation(data$M_between)) paths <- c(paths, "b_between")
  unique(paths)
}

.gp3b_med_validate_sampling <- function(
  chains, iter, warmup, cores, seed, adapt_delta, max_treedepth
) {
  ints <- list(chains = c(chains, 2L), iter = c(iter, 100L), warmup = c(warmup, 0L),
               cores = c(cores, 1L), max_treedepth = c(max_treedepth, 5L))
  for (name in names(ints)) {
    value <- ints[[name]][[1L]]
    minimum <- ints[[name]][[2L]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value != floor(value) || value < minimum) {
      .gp3b_med_stop("`", name, "` must be an integer >= ", minimum, ".")
    }
  }
  if (warmup >= iter) .gp3b_med_stop("`warmup` must be smaller than `iter`.")
  if (cores > chains) .gp3b_med_stop("`cores` cannot exceed `chains`.")
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != floor(seed) || seed < 0) {
    .gp3b_med_stop("`seed` must be a non-negative integer.")
  }
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L || is.na(adapt_delta) ||
      !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    .gp3b_med_stop("`adapt_delta` must lie strictly between 0 and 1.")
  }
  invisible(TRUE)
}

#' Specify Trial-Level Multilevel Gaze Mediation
#'
#' @export
specify_multilevel_gaze_mediation <- function(
  prepared,
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = "mediator_x",
  missingness_policy = "error"
) {
  prepared <- .gp3b_med_prepared(prepared)
  mediator_family <- .gp3b_med_family(mediator_family, "mediator")
  outcome_family <- .gp3b_med_family(outcome_family, "outcome")
  if (!inherits(priors, "gp3bayes_mediation_prior_specification")) {
    .gp3b_med_stop("`priors` must be a `gp3bayes_mediation_prior_specification`.")
  }
  allowed_slopes <- c("mediator_x", "outcome_x", "outcome_m")
  if (any(!random_slopes %in% allowed_slopes) || anyDuplicated(random_slopes)) {
    .gp3b_med_stop("`random_slopes` contains an unsupported or duplicated value.")
  }
  columns <- prepared$columns
  participant <- columns$participant
  trial <- columns$trial
  mediator <- columns$mediator
  outcome <- columns$outcome
  required <- c(participant, trial, mediator, outcome, "X_within", "X_between", "M_within", "M_between")
  missing <- setdiff(required, names(prepared$data))
  if (length(missing)) .gp3b_med_stop("Prepared data is missing model columns: ", paste(missing, collapse = ", "), ".")
  selected <- .gp3b_med_select_rows(prepared$data, required, missingness_policy)
  data <- selected$data
  if (!nrow(data)) .gp3b_med_stop("No rows remain under the requested missingness policy.")
  if (length(unique(data[[participant]])) < 2L) .gp3b_med_stop("At least two participants are required.")
  .gp3b_med_validate_family_values(data[[mediator]], mediator_family, "mediator")
  .gp3b_med_validate_family_values(data[[outcome]], outcome_family, "outcome")
  estimable_paths <- .gp3b_med_estimable_simple_paths(data)
  if (!all(c("a_within", "b_within") %in% estimable_paths)) {
    .gp3b_med_stop(
      "The within-participant indirect effect is not estimable because X_within or M_within ",
      "has no usable variation in the analysis rows."
    )
  }
  slope_to_path <- c(
    mediator_x = "a_within", outcome_x = "cprime_within", outcome_m = "b_within"
  )
  unsupported <- random_slopes[!unname(slope_to_path[random_slopes]) %in% estimable_paths]
  if (length(unsupported)) {
    .gp3b_med_stop(
      "Requested random slope(s) are not estimable from the observed within-participant variation: ",
      paste(unsupported, collapse = ", "), "."
    )
  }
  structure(
    list(
      data = data,
      participant_col = participant,
      trial_col = trial,
      mediator_col = mediator,
      outcome_col = outcome,
      mediator_family = mediator_family,
      outcome_family = outcome_family,
      priors = priors,
      random_slopes = random_slopes,
      missingness_policy = missingness_policy,
      input_rows = nrow(prepared$data),
      analysis_rows = nrow(data),
      excluded_row_positions = selected$excluded,
      estimable_paths = estimable_paths,
      provenance = list(
        preparation = prepared$provenance,
        model_specification = list(
          model_kind = "simple",
          mediator_family = mediator_family,
          outcome_family = outcome_family,
          random_slopes = random_slopes,
          missingness_policy = missingness_policy,
          priors = unclass(priors),
          estimand_scale = "linear_predictor_product",
          estimable_paths = estimable_paths
        )
      ),
      model_kind = "simple",
      fit_performed = FALSE,
      fitting_engine = "none"
    ),
    class = "gp3bayes_multilevel_mediation_specification"
  )
}

.gp3b_med_brms_family <- function(family) {
  switch(
    family,
    gaussian = brms::gaussian(),
    lognormal = brms::lognormal(),
    gamma = brms::Gamma(link = "log"),
    beta = brms::Beta(link = "logit"),
    bernoulli = brms::bernoulli(link = "logit"),
    poisson = brms::poisson(link = "log"),
    negative_binomial = brms::negbinomial(link = "log"),
    ordinal = brms::cumulative(link = "logit"),
    .gp3b_med_stop("Unsupported brms family `", family, "`.")
  )
}

.gp3b_med_random_term <- function(response = c("mediator", "outcome"), slopes, participant) {
  response <- match.arg(response)
  terms <- "1"
  if (response == "mediator" && "mediator_x" %in% slopes) terms <- c(terms, "X_within")
  if (response == "outcome" && "outcome_x" %in% slopes) terms <- c(terms, "X_within")
  if (response == "outcome" && "outcome_m" %in% slopes) terms <- c(terms, "M_within")
  paste0("(", paste(terms, collapse = " + "), " | ", participant, ")")
}

.gp3b_med_prior_list <- function(specification) {
  p <- specification$priors
  responses <- c(specification$mediator_col, specification$outcome_col)
  out <- list()
  for (resp in responses) {
    out <- c(out, list(
      brms::set_prior(paste0("normal(0,", p$coefficient_sd, ")"), class = "b", resp = resp),
      brms::set_prior(paste0("normal(0,", p$intercept_sd, ")"), class = "Intercept", resp = resp),
      brms::set_prior(paste0("student_t(3,0,", p$group_sd_scale, ")"), class = "sd", resp = resp)
    ))
  }
  out
}

.gp3b_med_formula_simple <- function(specification) {
  m_random <- .gp3b_med_random_term("mediator", specification$random_slopes, specification$participant_col)
  y_random <- .gp3b_med_random_term("outcome", specification$random_slopes, specification$participant_col)
  m_terms <- character()
  if ("a_within" %in% specification$estimable_paths) m_terms <- c(m_terms, "X_within")
  if ("a_between" %in% specification$estimable_paths) m_terms <- c(m_terms, "X_between")
  y_terms <- character()
  if ("cprime_within" %in% specification$estimable_paths) y_terms <- c(y_terms, "X_within")
  if ("cprime_between" %in% specification$estimable_paths) y_terms <- c(y_terms, "X_between")
  if ("b_within" %in% specification$estimable_paths) y_terms <- c(y_terms, "M_within")
  if ("b_between" %in% specification$estimable_paths) y_terms <- c(y_terms, "M_between")
  m_text <- paste0(
    "`", specification$mediator_col, "` ~ ", paste(c(m_terms, m_random), collapse = " + ")
  )
  y_text <- paste0(
    "`", specification$outcome_col, "` ~ ", paste(c(y_terms, y_random), collapse = " + ")
  )
  list(
    mediator = brms::bf(stats::as.formula(m_text), family = .gp3b_med_brms_family(specification$mediator_family)),
    outcome = brms::bf(stats::as.formula(y_text), family = .gp3b_med_brms_family(specification$outcome_family))
  )
}

#' Fit Trial-Level Bayesian Multilevel Gaze Mediation
#'
#' @export
fit_multilevel_gaze_mediation <- function(
  prepared,
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = "mediator_x",
  missingness_policy = "error",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = min(chains, 2L),
  seed = 1,
  adapt_delta = 0.95,
  max_treedepth = 12,
  backend = c("cmdstanr", "rstan"),
  refresh = 0
) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    .gp3b_med_stop("Package `brms` is required to fit multilevel mediation models.")
  }
  backend <- match.arg(backend)
  .gp3b_med_validate_sampling(chains, iter, warmup, cores, seed, adapt_delta, max_treedepth)
  specification <- specify_multilevel_gaze_mediation(
    prepared,
    mediator_family = mediator_family,
    outcome_family = outcome_family,
    priors = priors,
    random_slopes = random_slopes,
    missingness_policy = missingness_policy
  )
  formulas <- .gp3b_med_formula_simple(specification)
  model_formula <- formulas$mediator + formulas$outcome + brms::set_rescor(FALSE)
  fit <- brms::brm(
    formula = model_formula,
    data = specification$data,
    prior = .gp3b_med_prior_list(specification),
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    backend = backend,
    refresh = refresh,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    save_pars = brms::save_pars(all = TRUE)
  )
  out <- structure(
    list(
      specification = specification,
      backend_fit = fit,
      backend = paste0("brms/", backend),
      sampling = list(
        chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
        adapt_delta = adapt_delta, max_treedepth = max_treedepth
      ),
      package_versions = list(
        brms = as.character(utils::packageVersion("brms")),
        posterior = if (requireNamespace("posterior", quietly = TRUE)) as.character(utils::packageVersion("posterior")) else "not_installed"
      ),
      fit_performed = TRUE
    ),
    class = "gp3bayes_multilevel_mediation_fit"
  )
  convergence <- check_mediation_convergence(out)
  if (!isTRUE(convergence$passed)) {
    warning(
      "Mediation fit failed one or more critical convergence checks. Indirect-effect extraction ",
      "will fail by default until diagnostics are resolved.",
      call. = FALSE
    )
  }
  out
}

.gp3b_med_draws <- function(fit) {
  if (!inherits(fit, "gp3bayes_multilevel_mediation_fit")) {
    .gp3b_med_stop("`fit` must inherit from `gp3bayes_multilevel_mediation_fit`.")
  }
  if (!requireNamespace("posterior", quietly = TRUE)) {
    .gp3b_med_stop("Package `posterior` is required to extract mediation draws.")
  }
  posterior::as_draws_df(fit$backend_fit)
}

.gp3b_med_find_coef <- function(draws, response, coefficient) {
  escaped <- gsub("([.\\+*?\[\]^$(){}=!<>|:\\-])", "\\\\\\1", coefficient)
  candidates <- grep(paste0("^b_.*_", escaped, "$"), names(draws), value = TRUE)
  if (!length(candidates)) {
    # univariate naming fallback
    candidates <- grep(paste0("^b_", escaped, "$"), names(draws), value = TRUE)
  }
  if (length(candidates) > 1L) {
    token <- gsub("[^[:alnum:]]", "", response)
    match <- candidates[grepl(token, gsub("[^[:alnum:]]", "", candidates), fixed = TRUE)]
    if (length(match) == 1L) candidates <- match
  }
  if (length(candidates) != 1L) {
    .gp3b_med_stop(
      "Could not uniquely identify posterior coefficient `", coefficient,
      "` for response `", response, "`."
    )
  }
  as.numeric(draws[[candidates[[1L]]]])
}

#' Check Mediation Convergence
#'
#' @export
check_mediation_convergence <- function(
  fit,
  rhat_max = 1.01,
  ess_bulk_min = 400,
  divergences_max = 0L,
  treedepth_hits_max = 0L
) {
  if (!inherits(fit, "gp3bayes_multilevel_mediation_fit")) {
    .gp3b_med_stop("`fit` must inherit from `gp3bayes_multilevel_mediation_fit`.")
  }
  max_rhat <- NA_real_
  min_ess <- NA_real_
  if (requireNamespace("posterior", quietly = TRUE)) {
    summary <- posterior::summarise_draws(fit$backend_fit, "rhat", "ess_bulk")
    max_rhat <- suppressWarnings(max(summary$rhat, na.rm = TRUE))
    min_ess <- suppressWarnings(min(summary$ess_bulk, na.rm = TRUE))
    if (!is.finite(max_rhat)) max_rhat <- NA_real_
    if (!is.finite(min_ess)) min_ess <- NA_real_
  }
  divergences <- 0L
  treedepth_hits <- 0L
  if (requireNamespace("brms", quietly = TRUE)) {
    np <- tryCatch(brms::nuts_params(fit$backend_fit), error = function(e) NULL)
    if (!is.null(np)) {
      if ("Parameter" %in% names(np)) {
        divergences <- sum(np$Parameter == "divergent__" & np$Value > 0)
        max_td <- fit$sampling$max_treedepth
        treedepth_hits <- sum(np$Parameter == "treedepth__" & np$Value >= max_td)
      }
    }
  }
  issues <- character()
  if (!is.finite(max_rhat)) issues <- c(issues, "R-hat was not available") else if (max_rhat > rhat_max) issues <- c(issues, sprintf("max R-hat %.3f exceeds %.3f", max_rhat, rhat_max))
  if (!is.finite(min_ess)) issues <- c(issues, "bulk ESS was not available") else if (min_ess < ess_bulk_min) issues <- c(issues, sprintf("minimum bulk ESS %.0f is below %.0f", min_ess, ess_bulk_min))
  if (divergences > divergences_max) issues <- c(issues, paste0(divergences, " divergence(s) detected"))
  if (treedepth_hits > treedepth_hits_max) issues <- c(issues, paste0(treedepth_hits, " maximum-tree-depth hit(s) detected"))
  structure(
    list(
      status = if (length(issues)) "fail" else "pass",
      passed = !length(issues),
      max_rhat = max_rhat,
      min_ess_bulk = min_ess,
      divergences = divergences,
      treedepth_hits = treedepth_hits,
      thresholds = list(
        rhat_max = rhat_max, ess_bulk_min = ess_bulk_min,
        divergences_max = divergences_max, treedepth_hits_max = treedepth_hits_max
      ),
      issues = issues
    ),
    class = "gp3bayes_mediation_convergence"
  )
}

.gp3b_med_require_convergence <- function(fit, require_convergence) {
  if (isTRUE(require_convergence)) {
    check <- check_mediation_convergence(fit)
    if (!check$passed) {
      .gp3b_med_stop(
        "Critical convergence checks failed: ", paste(check$issues, collapse = "; "),
        ". Set `require_convergence = FALSE` only for diagnostic inspection."
      )
    }
  }
}

.gp3b_med_effect <- function(name, draws, probability, scale) {
  alpha <- (1 - probability) / 2
  structure(
    list(
      effect = name,
      draws = draws,
      scale = scale,
      probability = probability,
      mean = mean(draws),
      median = stats::median(draws),
      sd = stats::sd(draws),
      lower = unname(stats::quantile(draws, alpha)),
      upper = unname(stats::quantile(draws, 1 - alpha)),
      probability_positive = mean(draws > 0),
      probability_negative = mean(draws < 0)
    ),
    class = "gp3bayes_mediation_effect"
  )
}

#' Extract Posterior Indirect Effect
#'
#' @export
posterior_indirect_effect <- function(
  fit,
  level = c("within", "between"),
  probability = 0.95,
  require_convergence = TRUE
) {
  level <- match.arg(level)
  .gp3b_med_require_convergence(fit, require_convergence)
  draws <- .gp3b_med_draws(fit)
  spec <- fit$specification
  needed <- c(paste0("a_", level), paste0("b_", level))
  if (!all(needed %in% spec$estimable_paths)) {
    .gp3b_med_stop("The ", level, "-participant indirect effect is not estimable from the observed design.")
  }
  a <- .gp3b_med_find_coef(draws, spec$mediator_col, paste0("X_", level))
  b <- .gp3b_med_find_coef(draws, spec$outcome_col, paste0("M_", level))
  .gp3b_med_effect(paste0("indirect_", level), a * b, probability, "linear_predictor_product")
}

#' @export
estimate_indirect_effect <- function(fit, level = c("within", "between"), ...) {
  posterior_indirect_effect(fit, level = match.arg(level), ...)
}

#' @export
estimate_within_indirect_effect <- function(fit, ...) posterior_indirect_effect(fit, level = "within", ...)

#' @export
estimate_between_indirect_effect <- function(fit, ...) posterior_indirect_effect(fit, level = "between", ...)

#' @export
posterior_direct_effect <- function(
  fit,
  level = c("within", "between"),
  probability = 0.95,
  require_convergence = TRUE
) {
  level <- match.arg(level)
  .gp3b_med_require_convergence(fit, require_convergence)
  draws <- .gp3b_med_draws(fit)
  spec <- fit$specification
  needed <- paste0("cprime_", level)
  if (!needed %in% spec$estimable_paths) {
    .gp3b_med_stop("The ", level, "-participant direct effect is not estimable from the observed design.")
  }
  values <- .gp3b_med_find_coef(draws, spec$outcome_col, paste0("X_", level))
  .gp3b_med_effect(paste0("cprime_", level), values, probability, "linear_predictor")
}

#' @export
posterior_total_effect <- function(
  fit,
  level = c("within", "between"),
  probability = 0.95,
  require_convergence = TRUE
) {
  level <- match.arg(level)
  direct <- posterior_direct_effect(fit, level, probability, require_convergence)
  indirect <- posterior_indirect_effect(fit, level, probability, require_convergence = FALSE)
  .gp3b_med_effect(paste0("total_", level), direct$draws + indirect$draws, probability, "linear_predictor")
}

#' Summarise Multilevel Mediation
#'
#' @export
summarise_multilevel_mediation <- function(fit, probability = 0.95, require_convergence = TRUE) {
  .gp3b_med_require_convergence(fit, require_convergence)
  effects <- list()
  for (level in c("within", "between")) {
    candidates <- list(
      function() posterior_indirect_effect(fit, level, probability, FALSE),
      function() posterior_direct_effect(fit, level, probability, FALSE),
      function() posterior_total_effect(fit, level, probability, FALSE)
    )
    for (candidate in candidates) {
      value <- tryCatch(candidate(), error = function(e) NULL)
      if (!is.null(value)) effects[[length(effects) + 1L]] <- value
    }
  }
  do.call(rbind, lapply(effects, function(x) {
    data.frame(
      effect = x$effect, scale = x$scale, mean = x$mean, median = x$median,
      sd = x$sd, lower = x$lower, upper = x$upper,
      probability_positive = x$probability_positive,
      probability_negative = x$probability_negative,
      stringsAsFactors = FALSE
    )
  }))
}

#' Posterior Predictive Check for Mediation
#'
#' @export
posterior_predictive_check_mediation <- function(fit, ndraws = 200) {
  if (!requireNamespace("brms", quietly = TRUE)) .gp3b_med_stop("Package `brms` is required.")
  list(
    mediator = brms::pp_check(fit$backend_fit, resp = fit$specification$mediator_col, ndraws = ndraws),
    outcome = brms::pp_check(fit$backend_fit, resp = fit$specification$outcome_col, ndraws = ndraws)
  )
}

#' Prior Predictive Check for Mediation
#'
#' @export
prior_predictive_check_mediation <- function(specification, ndraws = 200, seed = 1) {
  if (!inherits(specification, "gp3bayes_multilevel_mediation_specification")) {
    .gp3b_med_stop("`specification` must be a multilevel mediation specification.")
  }
  if (!requireNamespace("brms", quietly = TRUE)) .gp3b_med_stop("Package `brms` is required.")
  formulas <- .gp3b_med_formula_simple(specification)
  model_formula <- formulas$mediator + formulas$outcome + brms::set_rescor(FALSE)
  fit <- brms::brm(
    formula = model_formula,
    data = specification$data,
    prior = .gp3b_med_prior_list(specification),
    sample_prior = "only",
    chains = 2,
    iter = max(500, ndraws + 250),
    warmup = 250,
    seed = seed,
    refresh = 0
  )
  list(
    mediator = brms::pp_check(fit, resp = specification$mediator_col, ndraws = ndraws),
    outcome = brms::pp_check(fit, resp = specification$outcome_col, ndraws = ndraws)
  )
}

#' Compare Multilevel Mediation Models
#'
#' @export
.gp3b_med_same_comparison_observations <- function(reference, candidate) {
  if (!identical(reference$mediator_col, candidate$mediator_col) ||
      !identical(reference$outcome_col, candidate$outcome_col)) return(FALSE)
  if (!identical(reference$analysis_rows, candidate$analysis_rows)) return(FALSE)
  ref_keys <- reference$data[c(reference$participant_col, reference$trial_col)]
  cand_keys <- candidate$data[c(candidate$participant_col, candidate$trial_col)]
  names(ref_keys) <- names(cand_keys) <- c("participant", "trial")
  if (!identical(ref_keys, cand_keys)) return(FALSE)
  ref_values <- reference$data[c(reference$mediator_col, reference$outcome_col)]
  cand_values <- candidate$data[c(candidate$mediator_col, candidate$outcome_col)]
  names(ref_values) <- names(cand_values) <- c("mediator", "outcome")
  identical(ref_values, cand_values)
}

compare_multilevel_mediation_models <- function(...) {
  fits <- list(...)
  if (length(fits) < 2L) .gp3b_med_stop("Provide at least two fitted mediation models.")
  for (i in seq_along(fits)) {
    if (!inherits(fits[[i]], "gp3bayes_multilevel_mediation_fit") || is.null(fits[[i]]$backend_fit)) {
      label <- names(fits)[[i]]
      if (is.null(label) || is.na(label) || !nzchar(label)) label <- paste0("model_", i)
      .gp3b_med_stop("Model `", label, "` is not a fitted mediation object.")
    }
  }
  reference <- fits[[1L]]$specification
  reference_label <- names(fits)[[1L]]
  if (is.null(reference_label) || is.na(reference_label) || !nzchar(reference_label)) reference_label <- "model_1"
  if (length(fits) > 1L) {
    for (i in 2:length(fits)) {
      if (!.gp3b_med_same_comparison_observations(reference, fits[[i]]$specification)) {
        label <- names(fits)[[i]]
        if (is.null(label) || is.na(label) || !nzchar(label)) label <- paste0("model_", i)
        .gp3b_med_stop(
          "PSIS-LOO comparison requires the same mediator/outcome observations in the same ",
          "participant-trial order; `", label, "` does not match `", reference_label, "`. ",
          "Do not compare fits produced from different missingness/exclusion sets."
        )
      }
    }
  }
  if (!requireNamespace("loo", quietly = TRUE)) .gp3b_med_stop("Package `loo` is required.")
  loos <- lapply(fits, function(x) brms::loo(x$backend_fit))
  loo::loo_compare(loos)
}

#' Plot Mediation Posterior Intervals
#'
#' @export
plot_mediation_posteriors <- function(fit, probability = 0.95, require_convergence = TRUE) {
  summary <- summarise_multilevel_mediation(fit, probability, require_convergence)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(summary)
  ggplot2::ggplot(summary, ggplot2::aes(x = mean, y = reorder(effect, mean))) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.15) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::labs(x = "Posterior effect", y = NULL, title = "Multilevel mediation posterior intervals")
}

#' Plot an Indirect-Effect Posterior
#'
#' @export
plot_indirect_effect_distribution <- function(
  fit,
  level = c("within", "between"),
  probability = 0.95,
  require_convergence = TRUE
) {
  effect <- posterior_indirect_effect(fit, match.arg(level), probability, require_convergence)
  data <- data.frame(value = effect$draws)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(data)
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::labs(x = paste(effect$effect, "(", effect$scale, ")"), y = "Posterior density")
}

#' Plot Participant-Specific Within-Participant Indirect Effects
#'
#' Participant-specific indirect effects are available only when both the
#' mediator `X_within` slope and the outcome `M_within` slope vary by
#' participant. The function never manufactures participant effects from a
#' model that estimated only one (or neither) of those random slopes.
#'
#' @param fit A fitted simple multilevel mediation model.
#'
#' @return A `ggplot2` plot when `ggplot2` is installed; otherwise a data frame
#'   of participant identifiers and posterior mean indirect effects.
#'
#' @export
plot_participant_mediation_effects <- function(fit) {
  if (!inherits(fit, "gp3bayes_multilevel_mediation_fit")) {
    .gp3b_med_stop("`fit` must inherit from `gp3bayes_multilevel_mediation_fit`.")
  }
  spec <- fit$specification
  needed_slopes <- c("mediator_x", "outcome_m")
  if (!all(needed_slopes %in% spec$random_slopes)) {
    .gp3b_med_stop(
      "Participant-specific indirect effects are unavailable because the model did not estimate ",
      "both participant-specific a and b paths jointly."
    )
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    .gp3b_med_stop("Package `brms` is required to extract participant-specific coefficients.")
  }

  coefficients <- brms::coef(fit$backend_fit, summary = FALSE)
  participant <- spec$participant_col
  if (is.null(coefficients[[participant]])) {
    .gp3b_med_stop("Participant-level coefficient draws were unavailable from the fitted backend model.")
  }
  array <- coefficients[[participant]]
  if (length(dim(array)) != 3L) {
    .gp3b_med_stop("Participant-level coefficient draws had an unexpected backend shape.")
  }
  effect_names <- dimnames(array)[[3L]]
  participant_names <- dimnames(array)[[2L]]
  if (is.null(effect_names) || is.null(participant_names)) {
    .gp3b_med_stop("Participant-level coefficient draws lacked required dimension names.")
  }

  normalize <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
  find_effect <- function(response, coefficient) {
    effect_norm <- normalize(effect_names)
    response_norm <- normalize(response)
    coefficient_norm <- normalize(coefficient)
    exact_targets <- c(
      paste0(response_norm, coefficient_norm),
      paste0(coefficient_norm, response_norm)
    )
    candidates <- which(effect_norm %in% exact_targets)
    if (length(candidates) != 1L) {
      candidates <- which(
        grepl(response_norm, effect_norm, fixed = TRUE) &
          grepl(coefficient_norm, effect_norm, fixed = TRUE)
      )
    }
    if (length(candidates) != 1L) {
      .gp3b_med_stop(
        "Could not uniquely identify participant coefficient `", coefficient,
        "` for response `", response, "`."
      )
    }
    candidates[[1L]]
  }

  a_index <- find_effect(spec$mediator_col, "X_within")
  b_index <- find_effect(spec$outcome_col, "M_within")
  draws <- dim(array)[[1L]]
  participants <- dim(array)[[2L]]
  a <- matrix(array[, , a_index], nrow = draws, ncol = participants)
  b <- matrix(array[, , b_index], nrow = draws, ncol = participants)
  values <- data.frame(
    participant = participant_names,
    posterior_mean_indirect = colMeans(a * b),
    stringsAsFactors = FALSE
  )
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(values)
  ggplot2::ggplot(values, ggplot2::aes(x = participant, y = posterior_mean_indirect)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      x = "Participant",
      y = "Posterior mean indirect effect",
      title = "Participant-specific within-participant indirect effects"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
}

#' Report Multilevel Gaze Mediation
#'
#' @export
report_multilevel_gaze_mediation <- function(fit, probability = 0.95, require_convergence = TRUE) {
  check <- check_mediation_convergence(fit)
  if (isTRUE(require_convergence) && !check$passed) {
    .gp3b_med_stop("A substantive report is blocked because convergence checks failed.")
  }
  within <- posterior_indirect_effect(fit, "within", probability, FALSE)
  between <- tryCatch(
    posterior_indirect_effect(fit, "between", probability, FALSE),
    error = function(e) NULL
  )
  s <- fit$specification
  between_text <- if (is.null(between)) {
    "the between-participant indirect effect was not estimable from the observed design"
  } else {
    sprintf(
      "the between-participant indirect effect was %.3f [%.3f, %.3f]",
      between$mean, between$lower, between$upper
    )
  }
  paste0(
    "A Bayesian trial-level multilevel mediation model was fit to ", s$analysis_rows,
    " observations from ", length(unique(s$data[[s$participant_col]])), " participants, using a ",
    s$mediator_family, " mediator model and ", s$outcome_family, " outcome model. The explicit ",
    "missingness policy was '", s$missingness_policy, "' (", length(s$excluded_row_positions),
    " input rows excluded). The within-participant indirect effect on the linear-predictor product ",
    "scale was ", sprintf("%.3f [%.3f, %.3f]", within$mean, within$lower, within$upper),
    ", and ", between_text, ". Sampler diagnostics status: ", check$status,
    ". For nonlinear families these coefficient products should not be interpreted as ",
    "probability-scale natural indirect effects."
  )
}

#' Simulate a Trial-Level Gaze Mediation Experiment
#'
#' @export
simulate_multilevel_gaze_mediation <- function(
  n_participants = 100L,
  trials_per_participant = 20L,
  a_within = 0.7,
  b_within = 0.8,
  cprime_within = 0.2,
  a_between = 0.2,
  b_between = 0.3,
  seed = 2026L
) {
  if (n_participants < 2L || trials_per_participant < 2L) {
    .gp3b_med_stop("Simulation requires at least two participants and two trials per participant.")
  }
  set.seed(seed)
  u_m <- stats::rnorm(n_participants, 0, 0.5)
  u_y <- stats::rnorm(n_participants, 0, 0.5)
  propensity <- stats::rbeta(n_participants, 4, 4)
  rows <- vector("list", n_participants)
  for (i in seq_len(n_participants)) {
    x <- stats::rbinom(trials_per_participant, 1, propensity[[i]])
    xb <- mean(x)
    xw <- x - xb
    m <- pmax(0.02, 1.5 + a_within * xw + a_between * xb + u_m[[i]] + stats::rnorm(trials_per_participant, 0, 0.6))
    mb <- mean(m)
    mw <- m - mb
    eta <- -0.5 + cprime_within * xw + b_within * mw + b_between * mb + u_y[[i]]
    y <- stats::rbinom(trials_per_participant, 1, stats::plogis(eta))
    rows[[i]] <- data.frame(
      participant_id = sprintf("p%03d", i),
      trial_id = seq_len(trials_per_participant),
      ai_correct = x,
      source_dwell = m,
      correct_override = y,
      valid_fraction = 1,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# Serial mediation ------------------------------------------------------------

#' Specify Serial Multilevel Gaze Mediation
#'
#' Requires a second mediator already decomposed by
#' `eyeprocess::add_multilevel_mediation_component()`.
#' @export
specify_multilevel_serial_gaze_mediation <- function(
  prepared,
  mediator2_family = "gaussian",
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(),
  missingness_policy = "error"
) {
  prepared <- .gp3b_med_prepared(prepared)
  if (length(random_slopes)) {
    .gp3b_med_stop(
      "Serial mediation random slopes are not yet implemented; pass `random_slopes = character()` ",
      "instead of requesting slopes that would be silently ignored."
    )
  }
  needed_semantics <- c("mediator2", "mediator2_within", "mediator2_between")
  if (any(!needed_semantics %in% names(prepared$columns))) {
    .gp3b_med_stop("Serial mediation requires eyeprocess-prepared mediator2 semantics.")
  }
  base <- specify_multilevel_gaze_mediation(
    prepared,
    mediator_family = mediator_family,
    outcome_family = outcome_family,
    priors = priors,
    random_slopes = character(),
    missingness_policy = missingness_policy
  )
  m2 <- prepared$columns$mediator2
  m2w <- prepared$columns$mediator2_within
  m2b <- prepared$columns$mediator2_between
  required <- c(base$participant_col, base$trial_col, base$mediator_col, m2, base$outcome_col,
                "X_within", "X_between", "M_within", "M_between", m2w, m2b)
  selected <- .gp3b_med_select_rows(prepared$data, required, missingness_policy)
  base$data <- selected$data
  base$analysis_rows <- nrow(selected$data)
  base$excluded_row_positions <- selected$excluded
  base$model_kind <- "serial"
  base$serial <- TRUE
  base$mediator2_col <- m2
  base$mediator2_within_col <- m2w
  base$mediator2_between_col <- m2b
  base$mediator2_family <- .gp3b_med_family(mediator2_family, "mediator")
  .gp3b_med_validate_family_values(base$data[[m2]], base$mediator2_family, "mediator2")

  estimable <- character()
  if (.gp3b_med_has_variation(base$data$X_within)) estimable <- c(estimable, "a1_within", "a2_within", "cprime_within")
  if (.gp3b_med_has_variation(base$data$X_between)) estimable <- c(estimable, "a1_between", "a2_between", "cprime_between")
  if (.gp3b_med_has_variation(base$data$M_within)) estimable <- c(estimable, "d_within", "b1_within")
  if (.gp3b_med_has_variation(base$data$M_between)) estimable <- c(estimable, "d_between", "b1_between")
  if (.gp3b_med_has_variation(base$data[[m2w]])) estimable <- c(estimable, "b2_within")
  if (.gp3b_med_has_variation(base$data[[m2b]])) estimable <- c(estimable, "b2_between")
  base$estimable_paths <- unique(estimable)
  required_serial <- c("a1_within", "d_within", "b2_within")
  if (!all(required_serial %in% base$estimable_paths)) {
    .gp3b_med_stop(
      "The within-participant serial indirect effect is not estimable from the observed design; missing path(s): ",
      paste(setdiff(required_serial, base$estimable_paths), collapse = ", "), "."
    )
  }
  base$provenance$model_specification$model_kind <- "serial"
  base$provenance$model_specification$mediator2_family <- base$mediator2_family
  base$provenance$model_specification$estimable_paths <- base$estimable_paths
  base$provenance$model_specification$random_effects_scope <- "participant_random_intercepts"
  class(base) <- c("gp3bayes_serial_mediation_specification", class(base))
  base
}

.gp3b_med_formula_serial <- function(spec) {
  participant <- spec$participant_col
  p <- spec$estimable_paths
  m1_terms <- character()
  if ("a1_within" %in% p) m1_terms <- c(m1_terms, "X_within")
  if ("a1_between" %in% p) m1_terms <- c(m1_terms, "X_between")
  m2_terms <- character()
  if ("a2_within" %in% p) m2_terms <- c(m2_terms, "X_within")
  if ("a2_between" %in% p) m2_terms <- c(m2_terms, "X_between")
  if ("d_within" %in% p) m2_terms <- c(m2_terms, "M_within")
  if ("d_between" %in% p) m2_terms <- c(m2_terms, "M_between")
  y_terms <- character()
  if ("cprime_within" %in% p) y_terms <- c(y_terms, "X_within")
  if ("cprime_between" %in% p) y_terms <- c(y_terms, "X_between")
  if ("b1_within" %in% p) y_terms <- c(y_terms, "M_within")
  if ("b1_between" %in% p) y_terms <- c(y_terms, "M_between")
  if ("b2_within" %in% p) y_terms <- c(y_terms, paste0("`", spec$mediator2_within_col, "`"))
  if ("b2_between" %in% p) y_terms <- c(y_terms, paste0("`", spec$mediator2_between_col, "`"))
  random <- paste0("(1 | ", participant, ")")
  m1 <- brms::bf(
    stats::as.formula(paste0("`", spec$mediator_col, "` ~ ", paste(c(m1_terms, random), collapse = " + "))),
    family = .gp3b_med_brms_family(spec$mediator_family)
  )
  m2 <- brms::bf(
    stats::as.formula(paste0("`", spec$mediator2_col, "` ~ ", paste(c(m2_terms, random), collapse = " + "))),
    family = .gp3b_med_brms_family(spec$mediator2_family)
  )
  y <- brms::bf(
    stats::as.formula(paste0("`", spec$outcome_col, "` ~ ", paste(c(y_terms, random), collapse = " + "))),
    family = .gp3b_med_brms_family(spec$outcome_family)
  )
  m1 + m2 + y + brms::set_rescor(FALSE)
}

#' Fit Serial Multilevel Gaze Mediation
#'
#' @export
fit_multilevel_serial_gaze_mediation <- function(
  prepared,
  mediator2_family = "gaussian",
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(),
  missingness_policy = "error",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = min(chains, 2L),
  seed = 1,
  adapt_delta = 0.95,
  max_treedepth = 12,
  backend = c("cmdstanr", "rstan"),
  refresh = 0
) {
  if (!requireNamespace("brms", quietly = TRUE)) .gp3b_med_stop("Package `brms` is required.")
  backend <- match.arg(backend)
  .gp3b_med_validate_sampling(chains, iter, warmup, cores, seed, adapt_delta, max_treedepth)
  spec <- specify_multilevel_serial_gaze_mediation(
    prepared = prepared, mediator2_family = mediator2_family, mediator_family = mediator_family,
    outcome_family = outcome_family, priors = priors, random_slopes = random_slopes,
    missingness_policy = missingness_policy
  )
  formula <- .gp3b_med_formula_serial(spec)
  fit <- brms::brm(
    formula, data = spec$data,
    chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
    backend = backend, refresh = refresh,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    save_pars = brms::save_pars(all = TRUE)
  )
  structure(
    list(
      specification = spec,
      backend_fit = fit,
      backend = paste0("brms/", backend),
      sampling = list(chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
                      adapt_delta = adapt_delta, max_treedepth = max_treedepth),
      fit_performed = TRUE
    ),
    class = c("gp3bayes_serial_mediation_fit", "gp3bayes_multilevel_mediation_fit")
  )
}

#' Extract a Serial Indirect Effect
#'
#' @export
posterior_serial_indirect_effect <- function(
  fit,
  level = c("within", "between"),
  probability = 0.95,
  require_convergence = TRUE
) {
  level <- match.arg(level)
  if (!inherits(fit, "gp3bayes_serial_mediation_fit")) .gp3b_med_stop("`fit` is not a serial mediation fit.")
  .gp3b_med_require_convergence(fit, require_convergence)
  draws <- .gp3b_med_draws(fit)
  s <- fit$specification
  needed <- if (level == "within") c("a1_within", "d_within", "b2_within") else c("a1_between", "d_between", "b2_between")
  if (!all(needed %in% s$estimable_paths)) {
    .gp3b_med_stop("The ", level, " serial indirect effect is not estimable from the observed design.")
  }
  a1 <- .gp3b_med_find_coef(draws, s$mediator_col, paste0("X_", level))
  d <- .gp3b_med_find_coef(draws, s$mediator2_col, paste0("M_", level))
  b2 <- .gp3b_med_find_coef(draws, s$outcome_col, if (level == "within") s$mediator2_within_col else s$mediator2_between_col)
  .gp3b_med_effect(paste0("serial_indirect_", level), a1 * d * b2, probability, "linear_predictor_product")
}

# Moderated mediation ---------------------------------------------------------

#' Specify Moderated Multilevel Gaze Mediation
#'
#' @export
specify_multilevel_moderated_gaze_mediation <- function(
  prepared,
  moderation_path = c("a", "b"),
  moderator_component = c("within", "between"),
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(),
  missingness_policy = "error"
) {
  moderation_path <- match.arg(moderation_path)
  moderator_component <- match.arg(moderator_component)
  if (length(random_slopes)) {
    .gp3b_med_stop(
      "Moderated mediation random slopes are not yet implemented; pass `random_slopes = character()` ",
      "instead of requesting slopes that would be silently ignored."
    )
  }
  prepared <- .gp3b_med_prepared(prepared)
  key <- paste0("moderator_", moderator_component)
  if (!key %in% names(prepared$columns)) {
    .gp3b_med_stop("Moderated mediation requires eyeprocess-prepared `", key, "` semantics.")
  }
  base <- specify_multilevel_gaze_mediation(
    prepared, mediator_family = mediator_family, outcome_family = outcome_family,
    priors = priors, random_slopes = character(), missingness_policy = missingness_policy
  )
  base$model_kind <- "moderated"
  base$moderated <- TRUE
  base$moderation_path <- moderation_path
  base$moderator_component <- moderator_component
  base$moderator_col <- prepared$columns[[key]]
  moderator_missing <- is.na(base$data[[base$moderator_col]])
  if (any(moderator_missing)) {
    if (missingness_policy == "error") {
      .gp3b_med_stop("Moderator contains missing values; choose an explicit missingness policy.")
    }
    source_rows <- as.integer(rownames(base$data))
    if (anyNA(source_rows)) source_rows <- seq_len(nrow(base$data))
    base$excluded_row_positions <- sort(unique(c(base$excluded_row_positions, source_rows[moderator_missing])))
    base$data <- base$data[!moderator_missing, , drop = FALSE]
    base$analysis_rows <- nrow(base$data)
  }
  if (!nrow(base$data)) .gp3b_med_stop("No rows remain after applying moderator missingness rules.")
  if (!.gp3b_med_has_variation(base$data[[base$moderator_col]])) {
    .gp3b_med_stop("The selected moderator component has no observed variation.")
  }
  interaction <- if (moderation_path == "a") {
    base$data$X_within * base$data[[base$moderator_col]]
  } else {
    base$data$M_within * base$data[[base$moderator_col]]
  }
  if (!.gp3b_med_has_variation(interaction)) {
    .gp3b_med_stop("The requested moderated path interaction has no observed variation.")
  }
  base$estimable_paths <- .gp3b_med_estimable_simple_paths(base$data)
  if (!all(c("a_within", "b_within") %in% base$estimable_paths)) {
    .gp3b_med_stop("The within-participant indirect effect is not estimable after moderator filtering.")
  }
  base$provenance$model_specification$model_kind <- "moderated"
  base$provenance$model_specification$moderation_path <- moderation_path
  base$provenance$model_specification$moderator_component <- moderator_component
  base$provenance$model_specification$estimable_paths <- base$estimable_paths
  base$provenance$model_specification$random_effects_scope <- "participant_random_intercepts"
  class(base) <- c("gp3bayes_moderated_mediation_specification", class(base))
  base
}

.gp3b_med_formula_moderated <- function(spec) {
  z <- paste0("`", spec$moderator_col, "`")
  participant <- spec$participant_col
  p <- spec$estimable_paths
  m_terms <- character()
  if ("a_within" %in% p) m_terms <- c(m_terms, "X_within")
  if ("a_between" %in% p) m_terms <- c(m_terms, "X_between")
  y_terms <- character()
  if ("cprime_within" %in% p) y_terms <- c(y_terms, "X_within")
  if ("cprime_between" %in% p) y_terms <- c(y_terms, "X_between")
  if ("b_within" %in% p) y_terms <- c(y_terms, "M_within")
  if ("b_between" %in% p) y_terms <- c(y_terms, "M_between")
  if (spec$moderation_path == "a") m_terms <- c(m_terms, paste0("X_within:", z))
  if (spec$moderation_path == "b") y_terms <- c(y_terms, paste0("M_within:", z))
  random <- paste0("(1 | ", participant, ")")
  m <- brms::bf(
    stats::as.formula(paste0("`", spec$mediator_col, "` ~ ", paste(c(m_terms, random), collapse = " + "))),
    family = .gp3b_med_brms_family(spec$mediator_family)
  )
  y <- brms::bf(
    stats::as.formula(paste0("`", spec$outcome_col, "` ~ ", paste(c(y_terms, random), collapse = " + "))),
    family = .gp3b_med_brms_family(spec$outcome_family)
  )
  m + y + brms::set_rescor(FALSE)
}

#' Fit Moderated Multilevel Gaze Mediation
#'
#' @export
fit_multilevel_moderated_gaze_mediation <- function(
  prepared,
  moderation_path = c("a", "b"),
  moderator_component = c("within", "between"),
  mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(),
  missingness_policy = "error",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = min(chains, 2L),
  seed = 1,
  adapt_delta = 0.95,
  max_treedepth = 12,
  backend = c("cmdstanr", "rstan"),
  refresh = 0
) {
  if (!requireNamespace("brms", quietly = TRUE)) .gp3b_med_stop("Package `brms` is required.")
  backend <- match.arg(backend)
  .gp3b_med_validate_sampling(chains, iter, warmup, cores, seed, adapt_delta, max_treedepth)
  spec <- specify_multilevel_moderated_gaze_mediation(
    prepared = prepared, moderation_path = moderation_path, moderator_component = moderator_component,
    mediator_family = mediator_family, outcome_family = outcome_family, priors = priors,
    random_slopes = random_slopes, missingness_policy = missingness_policy
  )
  fit <- brms::brm(
    .gp3b_med_formula_moderated(spec), data = spec$data,
    chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
    backend = backend, refresh = refresh,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    save_pars = brms::save_pars(all = TRUE)
  )
  structure(
    list(
      specification = spec,
      backend_fit = fit,
      backend = paste0("brms/", backend),
      sampling = list(chains = chains, iter = iter, warmup = warmup, cores = cores, seed = seed,
                      adapt_delta = adapt_delta, max_treedepth = max_treedepth),
      fit_performed = TRUE
    ),
    class = c("gp3bayes_moderated_mediation_fit", "gp3bayes_multilevel_mediation_fit")
  )
}

#' Conditional Within-Participant Indirect Effect
#'
#' @export
posterior_conditional_indirect_effect <- function(
  fit,
  moderator_value,
  probability = 0.95,
  require_convergence = TRUE
) {
  if (!inherits(fit, "gp3bayes_moderated_mediation_fit")) .gp3b_med_stop("`fit` is not a moderated mediation fit.")
  .gp3b_med_require_convergence(fit, require_convergence)
  draws <- .gp3b_med_draws(fit)
  s <- fit$specification
  a <- .gp3b_med_find_coef(draws, s$mediator_col, "X_within")
  b <- .gp3b_med_find_coef(draws, s$outcome_col, "M_within")
  interaction_coef <- if (s$moderation_path == "a") {
    paste0("X_within:", s$moderator_col)
  } else {
    paste0("M_within:", s$moderator_col)
  }
  mod <- .gp3b_med_find_coef(
    draws,
    if (s$moderation_path == "a") s$mediator_col else s$outcome_col,
    interaction_coef
  )
  if (!is.numeric(moderator_value) || length(moderator_value) != 1L || is.na(moderator_value) || !is.finite(moderator_value)) {
    .gp3b_med_stop("`moderator_value` must be one finite numeric value.")
  }
  z <- as.numeric(moderator_value)
  values <- if (s$moderation_path == "a") (a + mod * z) * b else a * (b + mod * z)
  .gp3b_med_effect(
    paste0("conditional_indirect_within_z_", z), values, probability, "linear_predictor_product"
  )
}

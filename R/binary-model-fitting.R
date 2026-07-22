.gp3b_require_namespace <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .gp3b_stop(
      "Optional package `",
      package,
      "` is required to ",
      purpose,
      "."
    )
  }

  invisible(TRUE)
}

.gp3b_validate_binary_model_specification <- function(specification) {
  if (
    !inherits(
      specification,
      "gp3bayes_binary_model_specification"
    )
  ) {
    .gp3b_stop(
      "`specification` must inherit from ",
      "`gp3bayes_binary_model_specification`."
    )
  }

  if (!identical(specification$family, "binary")) {
    .gp3b_stop(
      "`specification` must use the approved binary family."
    )
  }

  .gp3b_validate_contract(
    specification$contract,
    binary = TRUE
  )

  if (
    !inherits(
      specification$prepared,
      "gp3bayes_binary_prepared"
    )
  ) {
    .gp3b_stop(
      "`specification$prepared` must inherit from ",
      "`gp3bayes_binary_prepared`."
    )
  }

  if (!isTRUE(specification$audit$ready)) {
    .gp3b_stop(
      "`specification$audit` must pass the readiness gate."
    )
  }

  if (!identical(specification$contract$link, "logit")) {
    .gp3b_stop(
      "Binary model fitting requires the approved logit link."
    )
  }

  if (!identical(specification$contract$likelihood, "Bernoulli")) {
    .gp3b_stop(
      "Binary model fitting requires the approved Bernoulli likelihood."
    )
  }

  invisible(specification)
}

.gp3b_prior_number <- function(value) {
  format(
    as.numeric(value),
    digits = 15L,
    scientific = FALSE,
    trim = TRUE
  )
}

.gp3b_default_cores <- function(chains) {
  detected <- parallel::detectCores(
    logical = FALSE
  )

  if (
    length(detected) != 1L ||
      is.na(detected) ||
      !is.finite(detected) ||
      detected < 1
  ) {
    detected <- 1L
  }

  min(
    as.integer(chains),
    as.integer(detected)
  )
}

.gp3b_binary_prior_text <- function(specification) {
  intercept_prior <- .gp3b_prior_row(
    specification$priors,
    "Intercept"
  )

  coefficient_prior <- .gp3b_prior_row(
    specification$priors,
    "b"
  )

  group_sd_prior <- .gp3b_prior_row(
    specification$priors,
    "sd"
  )

  prior_text <- c(
    Intercept = paste0(
      "normal(",
      .gp3b_prior_number(
        intercept_prior$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        intercept_prior$scale[[1L]]
      ),
      ")"
    ),
    b = paste0(
      "normal(",
      .gp3b_prior_number(
        coefficient_prior$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        coefficient_prior$scale[[1L]]
      ),
      ")"
    ),
    sd = paste0(
      "student_t(",
      .gp3b_prior_number(
        group_sd_prior$df[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        group_sd_prior$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        group_sd_prior$scale[[1L]]
      ),
      ")"
    )
  )

  if (isTRUE(specification$contract$random_slope)) {
    correlation_prior <- .gp3b_prior_row(
      specification$priors,
      "cor"
    )

    prior_text <- c(
      prior_text,
      cor = paste0(
        "lkj(",
        .gp3b_prior_number(
          correlation_prior$shape[[1L]]
        ),
        ")"
      )
    )
  }

  prior_text
}

.gp3b_translate_binary_priors <- function(
  specification,
  prior_text
) {
  translated <- brms::set_prior(
    prior_text[["Intercept"]],
    class = "Intercept"
  )

  translated <- c(
    translated,
    brms::set_prior(
      prior_text[["b"]],
      class = "b"
    ),
    brms::set_prior(
      prior_text[["sd"]],
      class = "sd"
    )
  )

  if (isTRUE(specification$contract$random_slope)) {
    translated <- c(
      translated,
      brms::set_prior(
        prior_text[["cor"]],
        class = "cor"
      )
    )
  }

  translated
}

.gp3b_run_binary_brms <- function(
  translation,
  data,
  chains,
  iter,
  warmup,
  cores,
  seed,
  adapt_delta,
  max_treedepth,
  refresh
) {
  brms::brm(
    formula = translation$formula,
    data = data,
    family = translation$family_object,
    prior = translation$priors,
    backend = "rstan",
    algorithm = "sampling",
    sample_prior = "no",
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    ),
    refresh = refresh
  )
}

#' Translate a Binary Model Specification to brms
#'
#' Converts an approved backend-independent binary specification into a
#' restricted `brms` representation. The formula, Bernoulli-logit family, and
#' priors are derived entirely from the existing `gp3bayes` specification.
#'
#' @param specification A `gp3bayes_binary_model_specification`.
#'
#' @return A `gp3bayes_binary_backend_specification` containing the restricted
#'   formula, family, translated priors, validated prior table, and backend
#'   metadata.
#'
#' @details
#' This function performs translation and prior validation only. It does not
#' compile Stan code, run MCMC, create posterior draws, or assess convergence.
#' Users cannot supply an alternative formula, family, backend, algorithm, or
#' arbitrary backend arguments.
#'
#' @examples
#' if (requireNamespace("brms", quietly = TRUE)) {
#'   simulation <- simulate_hierarchical_binary_data(
#'     n_participants = 12,
#'     trials_per_participant = 8,
#'     n_items = 6,
#'     random_slope_sd = 0,
#'     seed = 2026
#'   )
#'
#'   contract <- create_model_contract(
#'     family = "binary",
#'     outcome_col = "selected",
#'     participant_col = "participant_id",
#'     item_col = "item_id",
#'     trial_col = "trial_id",
#'     condition_col = "condition"
#'   )
#'
#'   prepared <- prepare_hierarchical_binary_data(
#'     simulation$data,
#'     contract,
#'     condition_levels = c("control", "treatment")
#'   )
#'
#'   specification <- specify_binary_model(
#'     prepared,
#'     baseline = 0.35
#'   )
#'
#'   translate_binary_model_to_brms(specification)
#' }
#'
#' @export
translate_binary_model_to_brms <- function(specification) {
  .gp3b_validate_binary_model_specification(
    specification
  )

  .gp3b_require_namespace(
    "brms",
    "translate a binary model specification"
  )

  family_object <- brms::bernoulli(
    link = "logit"
  )

  prior_text <- .gp3b_binary_prior_text(
    specification
  )

  translated_priors <- .gp3b_translate_binary_priors(
    specification,
    prior_text
  )

  validated_priors <- brms::validate_prior(
    prior = translated_priors,
    formula = specification$formula,
    data = specification$prepared$data,
    family = family_object
  )

  parameter_table <- validated_priors
  class(parameter_table) <- "data.frame"

  required_user_classes <- c(
    "Intercept",
    "b",
    "sd"
  )

  observed_user_classes <- parameter_table$class[
    parameter_table$source == "user"
  ]

  if (!all(
    required_user_classes %in%
      observed_user_classes
  )) {
    .gp3b_stop(
      "The translated backend prior table does not contain all ",
      "required population-level and group-scale priors."
    )
  }

  if (isTRUE(specification$contract$random_slope)) {
    correlation_present <- any(
      parameter_table$class == "L" &
        parameter_table$source == "user" &
        !is.na(parameter_table$prior) &
        nzchar(parameter_table$prior)
    )

    if (!isTRUE(correlation_present)) {
      .gp3b_stop(
        "The translated backend prior table does not contain the ",
        "required participant correlation prior."
      )
    }
  }

  structure(
    list(
      translation_version = "0.1",
      family = "binary",
      model_family = "hierarchical_binary",
      formula = specification$formula,
      formula_text = specification$formula_text,
      family_object = family_object,
      priors = translated_priors,
      prior_text = prior_text,
      validated_priors = validated_priors,
      parameter_table = parameter_table,
      specification = specification,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      backend_available = requireNamespace(
        "rstan",
        quietly = TRUE
      ),
      unrestricted_formula = FALSE,
      compiled = FALSE,
      fit_performed = FALSE,
      diagnostics_assessed = FALSE
    ),
    class = "gp3bayes_binary_backend_specification"
  )
}

#' Fit an Approved Hierarchical Binary Model
#'
#' Fits an approved binary model specification using full MCMC sampling through
#' the fixed `brms` and `rstan` route.
#'
#' @param specification A `gp3bayes_binary_model_specification`.
#' @param chains Number of MCMC chains.
#' @param iter Total iterations per chain, including warmup.
#' @param warmup Warmup iterations per chain.
#' @param cores Number of processor cores. It cannot exceed `chains`.
#' @param seed Non-negative integer random-number seed.
#' @param adapt_delta Target acceptance probability for the No-U-Turn sampler.
#' @param max_treedepth Maximum tree depth for the No-U-Turn sampler.
#' @param refresh Console progress refresh interval. Use zero to suppress
#'   iteration progress output.
#'
#' @return A `gp3bayes_binary_fit` containing the fitted backend object,
#'   original specification, restricted translation, and recorded sampling
#'   settings.
#'
#' @details
#' The function fixes the likelihood to Bernoulli, the link to logit, the
#' interface to `brms`, the sampling backend to `rstan`, and the algorithm to
#' full MCMC sampling. It does not expose arbitrary backend arguments.
#'
#' A returned fit is not evidence of convergence, posterior adequacy, causal
#' identification, or substantive validity. Those assessments require separate
#' diagnostic and reporting gates.
#'
#' @examples
#' \dontrun{
#' fit <- fit_binary_model(
#'   specification,
#'   chains = 4,
#'   iter = 2000,
#'   warmup = 1000,
#'   cores = 4,
#'   seed = 2026
#' )
#' }
#'
#' @export
fit_binary_model <- function(
  specification,
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = .gp3b_default_cores(chains),
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
) {
  .gp3b_validate_binary_model_specification(
    specification
  )

  chains <- .gp3b_assert_integer(
    chains,
    "chains",
    minimum = 1L
  )

  iter <- .gp3b_assert_integer(
    iter,
    "iter",
    minimum = 100L
  )

  warmup <- .gp3b_assert_integer(
    warmup,
    "warmup",
    minimum = 0L
  )

  if (warmup >= iter) {
    .gp3b_stop(
      "`warmup` must be smaller than `iter`."
    )
  }

  cores <- .gp3b_assert_integer(
    cores,
    "cores",
    minimum = 1L
  )

  if (cores > chains) {
    .gp3b_stop(
      "`cores` cannot exceed `chains`."
    )
  }

  seed <- .gp3b_assert_integer(
    seed,
    "seed",
    minimum = 0L
  )

  adapt_delta <- .gp3b_assert_numeric_scalar(
    adapt_delta,
    "adapt_delta",
    lower = 0,
    upper = 1,
    lower_open = TRUE,
    upper_open = TRUE
  )

  max_treedepth <- .gp3b_assert_integer(
    max_treedepth,
    "max_treedepth",
    minimum = 5L
  )

  refresh <- .gp3b_assert_integer(
    refresh,
    "refresh",
    minimum = 0L
  )

  translation <- translate_binary_model_to_brms(
    specification
  )

  .gp3b_require_namespace(
    "rstan",
    "fit a binary model through the approved sampling backend"
  )

  backend_fit <- .gp3b_run_binary_brms(
    translation = translation,
    data = specification$prepared$data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )

  structure(
    list(
      fit_version = "0.1",
      family = "binary",
      model_family = "hierarchical_binary",
      specification = specification,
      translation = translation,
      backend_fit = backend_fit,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      sampling = list(
        chains = chains,
        iter = iter,
        warmup = warmup,
        post_warmup_iterations = iter - warmup,
        cores = cores,
        seed = seed,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth,
        refresh = refresh
      ),
      package_versions = c(
        brms = as.character(
          utils::packageVersion("brms")
        ),
        rstan = as.character(
          utils::packageVersion("rstan")
        )
      ),
      unrestricted_formula = FALSE,
      fit_performed = TRUE,
      diagnostics_assessed = FALSE,
      posterior_adequacy_established = FALSE
    ),
    class = c(
      "gp3bayes_binary_fit",
      "gp3bayes_fit"
    )
  )
}

#' @export
print.gp3bayes_binary_backend_specification <- function(
  x,
  ...
) {
  cat("<gp3bayes_binary_backend_specification>\n")
  cat(
    "  Formula: ",
    x$formula_text,
    "\n",
    sep = ""
  )
  cat("  Family: Bernoulli-logit\n")
  cat("  Interface: brms\n")
  cat("  Sampling backend: rstan\n")
  cat("  Algorithm: sampling\n")
  cat(
    "  Backend available: ",
    x$backend_available,
    "\n",
    sep = ""
  )
  cat("  Compiled: FALSE\n")
  cat("  Fit performed: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_binary_fit <- function(x, ...) {
  cat("<gp3bayes_binary_fit>\n")
  cat(
    "  Formula: ",
    x$translation$formula_text,
    "\n",
    sep = ""
  )
  cat("  Family: Bernoulli-logit\n")
  cat("  Interface: brms\n")
  cat("  Sampling backend: rstan\n")
  cat("  Algorithm: sampling\n")
  cat(
    "  Chains: ",
    x$sampling$chains,
    "\n",
    sep = ""
  )
  cat(
    "  Iterations per chain: ",
    x$sampling$iter,
    "\n",
    sep = ""
  )
  cat(
    "  Warmup per chain: ",
    x$sampling$warmup,
    "\n",
    sep = ""
  )
  cat("  Fit performed: TRUE\n")
  cat("  Diagnostics assessed: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}

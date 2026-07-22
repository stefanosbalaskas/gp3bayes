.gp3d_validate_duration_specification <- function(
  specification
) {
  if (!inherits(
    specification,
    "gp3bayes_duration_model_specification"
  )) {
    .gp3b_stop(
      "`specification` must inherit from ",
      "`gp3bayes_duration_model_specification`."
    )
  }

  .gp3d_validate_contract(
    specification$contract
  )

  if (!inherits(
    specification$prepared,
    "gp3bayes_duration_prepared"
  )) {
    .gp3b_stop(
      "`specification$prepared` must inherit from ",
      "`gp3bayes_duration_prepared`."
    )
  }

  if (!isTRUE(
    specification$audit$ready
  )) {
    .gp3b_stop(
      "`specification$audit` must pass the readiness gate."
    )
  }

  invisible(specification)
}

.gp3d_duration_prior_text <- function(
  specification
) {
  priors <- specification$priors

  intercept <- .gp3b_prior_row(
    priors,
    "Intercept"
  )
  coefficient <- .gp3b_prior_row(
    priors,
    "b"
  )
  group_sd <- .gp3b_prior_row(
    priors,
    "sd"
  )
  residual <- .gp3b_prior_row(
    priors,
    "sigma"
  )

  text <- c(
    Intercept = paste0(
      "normal(",
      .gp3b_prior_number(
        intercept$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        intercept$scale[[1L]]
      ),
      ")"
    ),
    b = paste0(
      "normal(",
      .gp3b_prior_number(
        coefficient$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        coefficient$scale[[1L]]
      ),
      ")"
    ),
    sd = paste0(
      "student_t(",
      .gp3b_prior_number(
        group_sd$df[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        group_sd$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        group_sd$scale[[1L]]
      ),
      ")"
    ),
    sigma = paste0(
      "student_t(",
      .gp3b_prior_number(
        residual$df[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        residual$location[[1L]]
      ),
      ", ",
      .gp3b_prior_number(
        residual$scale[[1L]]
      ),
      ")"
    )
  )

  if (isTRUE(
    specification$contract$random_slope
  )) {
    correlation <- .gp3b_prior_row(
      priors,
      "cor"
    )

    text <- c(
      text,
      cor = paste0(
        "lkj(",
        .gp3b_prior_number(
          correlation$shape[[1L]]
        ),
        ")"
      )
    )
  }

  text
}

.gp3d_translate_duration_priors <- function(
  specification,
  prior_text
) {
  translated <- c(
    brms::set_prior(
      prior_text[["Intercept"]],
      class = "Intercept"
    ),
    brms::set_prior(
      prior_text[["b"]],
      class = "b"
    ),
    brms::set_prior(
      prior_text[["sd"]],
      class = "sd"
    ),
    brms::set_prior(
      prior_text[["sigma"]],
      class = "sigma"
    )
  )

  if (isTRUE(
    specification$contract$random_slope
  )) {
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

.gp3d_run_duration_brms <- function(
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
    formula =
      translation$formula,
    data = data,
    family =
      translation$family_object,
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
      adapt_delta =
        adapt_delta,
      max_treedepth =
        max_treedepth
    ),
    refresh = refresh
  )
}

#' Translate a Duration Model Specification to brms
#'
#' Converts an approved backend-independent duration specification into a fixed
#' hierarchical lognormal `brms` representation.
#'
#' @param specification A `gp3bayes_duration_model_specification`.
#'
#' @return A `gp3bayes_duration_backend_specification`.
#'
#' @details
#' Translation validates the formula and priors but does not compile Stan code
#' or fit a model. Users cannot supply an alternative family, formula, backend,
#' algorithm, Stan extension, or arbitrary backend arguments.
#'
#' @export
translate_duration_model_to_brms <- function(
  specification
) {
  .gp3d_validate_duration_specification(
    specification
  )

  .gp3b_require_namespace(
    "brms",
    "translate a duration model specification"
  )

  family_object <- brms::lognormal(
    link = "identity"
  )

  prior_text <-
    .gp3d_duration_prior_text(
      specification
    )
  translated_priors <-
    .gp3d_translate_duration_priors(
      specification,
      prior_text
    )

  validated_priors <- brms::validate_prior(
    prior = translated_priors,
    formula = specification$formula,
    data =
      specification$prepared$data,
    family = family_object
  )

  parameter_table <- validated_priors
  class(parameter_table) <- "data.frame"

  required_classes <- c(
    "Intercept",
    "b",
    "sd",
    "sigma"
  )

  observed_user_classes <-
    parameter_table$class[
      parameter_table$source == "user"
    ]

  if (!all(
    required_classes %in%
      observed_user_classes
  )) {
    .gp3b_stop(
      "The duration translation is missing required user priors."
    )
  }

  if (isTRUE(
    specification$contract$random_slope
  )) {
    correlation_present <- any(
      parameter_table$class == "L" &
        parameter_table$source ==
          "user" &
        !is.na(
          parameter_table$prior
        ) &
        nzchar(
          parameter_table$prior
        )
    )

    if (!isTRUE(
      correlation_present
    )) {
      .gp3b_stop(
        "The duration translation is missing the required ",
        "participant correlation prior."
      )
    }
  }

  structure(
    list(
      translation_version = "0.1",
      family = "duration",
      model_family =
        "hierarchical_lognormal_duration",
      formula =
        specification$formula,
      formula_text =
        specification$formula_text,
      family_object =
        family_object,
      priors =
        translated_priors,
      prior_text =
        prior_text,
      validated_priors =
        validated_priors,
      parameter_table =
        parameter_table,
      specification =
        specification,
      outcome_unit =
        specification$outcome_unit,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      backend_available =
        requireNamespace(
          "rstan",
          quietly = TRUE
        ),
      unrestricted_formula = FALSE,
      compiled = FALSE,
      fit_performed = FALSE,
      diagnostics_assessed = FALSE
    ),
    class =
      "gp3bayes_duration_backend_specification"
  )
}

#' Fit an Approved Hierarchical Lognormal Duration Model
#'
#' Fits an approved strictly positive uncensored duration model using full MCMC
#' through the fixed `brms` and `rstan` route.
#'
#' @param specification A `gp3bayes_duration_model_specification`.
#' @param chains Number of MCMC chains.
#' @param iter Total iterations per chain.
#' @param warmup Warmup iterations per chain.
#' @param cores Processor cores, not exceeding `chains`.
#' @param seed Non-negative integer seed.
#' @param adapt_delta Target NUTS acceptance probability.
#' @param max_treedepth Maximum NUTS tree depth.
#' @param refresh Console progress refresh interval.
#'
#' @return A `gp3bayes_duration_fit`.
#'
#' @details
#' The likelihood is fixed to lognormal, the link to identity on the mean-log
#' parameter, the interface to `brms`, the backend to `rstan`, and the
#' algorithm to full MCMC sampling. A returned fit does not establish
#' convergence or posterior adequacy.
#'
#' @export
fit_duration_model <- function(
  specification,
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = .gp3b_default_cores(
    chains
  ),
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
) {
  .gp3d_validate_duration_specification(
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
  adapt_delta <-
    .gp3b_assert_numeric_scalar(
      adapt_delta,
      "adapt_delta",
      lower = 0,
      upper = 1,
      lower_open = TRUE,
      upper_open = TRUE
    )
  max_treedepth <-
    .gp3b_assert_integer(
      max_treedepth,
      "max_treedepth",
      minimum = 5L
    )
  refresh <- .gp3b_assert_integer(
    refresh,
    "refresh",
    minimum = 0L
  )

  translation <-
    translate_duration_model_to_brms(
      specification
    )

  .gp3b_require_namespace(
    "rstan",
    "fit a duration model through the approved sampling backend"
  )

  backend_fit <-
    .gp3d_run_duration_brms(
      translation =
        translation,
      data =
        specification$prepared$data,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      seed = seed,
      adapt_delta =
        adapt_delta,
      max_treedepth =
        max_treedepth,
      refresh = refresh
    )

  structure(
    list(
      fit_version = "0.1",
      family = "duration",
      model_family =
        "hierarchical_lognormal_duration",
      specification =
        specification,
      translation =
        translation,
      backend_fit =
        backend_fit,
      outcome_unit =
        specification$outcome_unit,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      sampling = list(
        chains = chains,
        iter = iter,
        warmup = warmup,
        post_warmup_iterations =
          iter - warmup,
        cores = cores,
        seed = seed,
        adapt_delta =
          adapt_delta,
        max_treedepth =
          max_treedepth,
        refresh = refresh
      ),
      package_versions = c(
        brms = as.character(
          utils::packageVersion(
            "brms"
          )
        ),
        rstan = as.character(
          utils::packageVersion(
            "rstan"
          )
        )
      ),
      unrestricted_formula = FALSE,
      fit_performed = TRUE,
      diagnostics_assessed = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class = c(
      "gp3bayes_duration_fit",
      "gp3bayes_fit"
    )
  )
}

#' @export
print.gp3bayes_duration_backend_specification <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_backend_specification>\n")
  cat(
    "  Formula: ",
    x$formula_text,
    "\n",
    sep = ""
  )
  cat("  Family: lognormal\n")
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
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
print.gp3bayes_duration_fit <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_fit>\n")
  cat(
    "  Formula: ",
    x$translation$formula_text,
    "\n",
    sep = ""
  )
  cat("  Family: lognormal\n")
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
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

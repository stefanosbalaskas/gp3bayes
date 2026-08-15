.gp3p_validate_sampling <- function(chains, iter, warmup, cores,
                                    adapt_delta, max_treedepth, refresh) {
  chains <- .gp3p_positive(chains, "chains", TRUE)
  iter <- .gp3p_positive(iter, "iter", TRUE)
  warmup <- .gp3p_positive(warmup, "warmup", TRUE)
  cores <- .gp3p_positive(cores, "cores", TRUE)
  max_treedepth <- .gp3p_positive(max_treedepth, "max_treedepth", TRUE)
  if (warmup >= iter) .gp3p_stop("`warmup` must be smaller than `iter`.")
  if (cores > 2L) .gp3p_stop("gp3bayes pupil fitting permits at most two package-controlled cores.")
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L ||
      is.na(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    .gp3p_stop("`adapt_delta` must be strictly between zero and one.")
  }
  if (!is.numeric(refresh) || length(refresh) != 1L || is.na(refresh) ||
      refresh < 0 || refresh != as.integer(refresh)) {
    .gp3p_stop("`refresh` must be a non-negative integer.")
  }
  list(chains = chains, iter = iter, warmup = warmup,
       cores = cores, adapt_delta = adapt_delta,
       max_treedepth = max_treedepth, refresh = as.integer(refresh))
}


.gp3p_new_pupil_fit <- function(specification, translation, backend_fit,
                                backend, sampling, package_versions) {
  structure(
    list(
      fit_version = "0.4-pupil-1",
      family = "pupil",
      model_family = "Gaussian",
      specification = specification,
      translation = translation,
      backend_fit = backend_fit,
      outcome_unit = specification$outcome_unit,
      backend_interface = "brms",
      sampling_backend = backend,
      algorithm = "sampling",
      sampling = sampling,
      package_versions = package_versions,
      unrestricted_formula = FALSE,
      unrestricted_family = FALSE,
      fit_performed = TRUE,
      convergence_established = FALSE,
      posterior_adequacy_established = FALSE,
      causal_identification_established = FALSE
    ),
    class = c(
      "gp3bayes_pupil_fit",
      "gp3bayes_backend_portable_fit",
      "gp3bayes_fit"
    )
  )
}

#' Fit the restricted pupil time-course model with an approved backend
#'
#' Fits only a `specify_pupil_timecourse_model()` specification through
#' `brms` using full MCMC sampling and either `rstan` or `cmdstanr`.
#'
#' @param specification Approved pupil model specification.
#' @param backend `"rstan"` or `"cmdstanr"`.
#' @param chains,iter,warmup,cores,seed Sampling controls. Package-controlled
#'   cores are capped at two.
#' @param adapt_delta,max_treedepth,refresh Fixed safe sampling controls.
#' @return A `gp3bayes_pupil_fit`.
#' @section Governance boundary:
#' This interface accepts no arbitrary formula, family, Stan program,
#' inference algorithm, or unrestricted backend arguments. A returned fit does
#' not establish convergence, adequacy, predictive validity, or psychological
#' interpretation.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
fit_pupil_model_backend <- function(
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

  if (!inherits(specification, "gp3bayes_pupil_model_specification")) {
    .gp3p_stop("`specification` must be created by `specify_pupil_timecourse_model()`.")
  }
  backend <- match.arg(backend)
  s <- .gp3p_validate_sampling(
    chains, iter, warmup, cores, adapt_delta, max_treedepth, refresh
  )
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) .gp3p_stop("`seed` must be one integer.")
  translation <- translate_pupil_model_to_brms(specification)

  if (identical(backend, "cmdstanr")) {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      .gp3p_stop("Package `cmdstanr` is required for the cmdstanr backend.")
    }
    path <- try(cmdstanr::cmdstan_path(), silent = TRUE)
    if (inherits(path, "try-error") || !nzchar(path)) {
      .gp3p_stop("A working CmdStan installation is required for `backend = \"cmdstanr\"`.")
    }
  } else if (!requireNamespace("rstan", quietly = TRUE)) {
    .gp3p_stop("Package `rstan` is required for the rstan backend.")
  }

  backend_fit <- brms::brm(
    formula = translation$formula,
    data = translation$data,
    family = translation$brms_family,
    prior = translation$prior,
    backend = backend,
    algorithm = "sampling",
    sample_prior = "no",
    save_pars = brms::save_pars(all = TRUE),
    chains = s$chains,
    iter = s$iter,
    warmup = s$warmup,
    cores = s$cores,
    seed = seed,
    refresh = s$refresh,
    control = list(
      adapt_delta = s$adapt_delta,
      max_treedepth = s$max_treedepth
    )
  )

  .gp3p_new_pupil_fit(
    specification = specification,
    translation = translation,
    backend_fit = backend_fit,
    backend = backend,
    sampling = c(s, list(seed = seed)),
    package_versions = c(
      brms = as.character(utils::packageVersion("brms")),
      backend = if (identical(backend, "rstan"))
        as.character(utils::packageVersion("rstan"))
      else as.character(utils::packageVersion("cmdstanr"))
    )
  )
}

#' Fit a pupil model through the fixed rstan route
#' @inheritParams fit_pupil_model_backend
#' @return A `gp3bayes_pupil_fit`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
fit_pupil_model <- function(
    specification, chains = 4L, iter = 2000L, warmup = 1000L,
    cores = min(2L, chains), seed = 2026, adapt_delta = 0.95,
    max_treedepth = 12L, refresh = 0L) {
  fit_pupil_model_backend(
    specification, backend = "rstan", chains = chains, iter = iter,
    warmup = warmup, cores = cores, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, refresh = refresh
  )
}

#' Fit a pupil model through the fixed cmdstanr route
#' @inheritParams fit_pupil_model_backend
#' @return A `gp3bayes_pupil_fit`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
fit_pupil_model_cmdstanr <- function(
    specification, chains = 4L, iter = 2000L, warmup = 1000L,
    cores = min(2L, chains), seed = 2026, adapt_delta = 0.95,
    max_treedepth = 12L, refresh = 0L) {
  fit_pupil_model_backend(
    specification, backend = "cmdstanr", chains = chains, iter = iter,
    warmup = warmup, cores = cores, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth, refresh = refresh
  )
}

#' @export
print.gp3bayes_pupil_fit <- function(x, ...) {
  cat("<gp3bayes_pupil_fit>\n")
  cat("  Family: Gaussian pupil time-course\n")
  cat("  Backend: ", x$sampling_backend, "\n", sep = "")
  cat("  Outcome unit: ", x$outcome_unit, "\n", sep = "")
  cat("  Algorithm: sampling\n")
  cat("  Unrestricted formula: FALSE\n")
  cat("  Fit performed: TRUE\n")
  cat("  Convergence established by fitting alone: FALSE\n")
  invisible(x)
}

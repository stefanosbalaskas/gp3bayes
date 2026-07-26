#' gp3bayes: Contract-First Bayesian Workflows for Hierarchical Behavioural Data
#'
#' `gp3bayes` provides package-neutral infrastructure for transparent,
#' contract-first Bayesian workflows for repeated-measures and hierarchical
#' behavioural data. It implements approved Bernoulli-logit and positive
#' lognormal duration workflows with deterministic simulation, recorded
#' preparation, inspectable priors, restricted optional full-MCMC fitting,
#' sampling diagnostics, posterior predictive checks, prior sensitivity,
#' simulation-based recovery, and conservative structured reporting.
#' Fitting or passing a numerical threshold does not by itself establish
#' convergence, posterior adequacy, causal identification, or validity.
#'
#' @section Approved model families:
#' The approved model-family scope is restricted to:
#'
#' * hierarchical Bernoulli-logit models for binary trial-level outcomes;
#' * hierarchical lognormal models for strictly positive uncensored
#'   durations.
#'
#' Additional outcome families require separate methodological approval.
#'
#' @section Backend policy:
#' Core validation, contract, simulation, preparation, transformation,
#' specification, and prior-predictive functionality remains usable without a
#' Bayesian backend. Restricted full-MCMC fitting uses the `brms` interface.
#' The original `fit_binary_model()` and `fit_duration_model()` interfaces
#' retain the fixed `rstan` route, while the backend-portable
#' `fit_binary_model_backend()` and `fit_duration_model_backend()` interfaces
#' support either `rstan` or `cmdstanr`. Model families, formulas, priors, and
#' algorithms remain contract-restricted.
#'
#' @section Interpretation boundaries:
#' Behavioural measurements do not directly reveal emotion, stress,
#' cognition, comprehension, personality, diagnosis, deception, intention,
#' or other latent psychological states. Associations must not be described
#' as causal effects unless the design and estimand justify that language.
#'
#' @keywords internal
"_PACKAGE"

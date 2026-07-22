#' gp3bayes: Contract-First Bayesian Workflows for Hierarchical Behavioural Data
#'
#' `gp3bayes` provides package-neutral infrastructure for transparent,
#' contract-first Bayesian workflows for repeated-measures and hierarchical
#' behavioural data. The package implements model contracts, readiness audits,
#' deterministic hierarchical binary simulation, recorded transformations,
#' restricted model specifications, inspectable priors, prior-predictive
#' checks, and optional full-MCMC fitting of approved binary models through
#' `brms` and `rstan`. Fitting alone does not establish convergence or
#' posterior adequacy.
#'
#' @section Initial model families:
#' The initial development scope is restricted to:
#'
#' * hierarchical Bernoulli-logit models for binary trial-level outcomes;
#' * hierarchical lognormal models for strictly positive uncensored
#'   durations.
#'
#' Additional outcome families require separate methodological approval.
#'
#' @section Backend policy:
#' Core validation, contract, simulation, preparation, and specification
#' functionality remains usable without a Bayesian backend. Binary model
#' fitting uses the optional `brms` interface with the fixed `rstan` sampling
#' backend through restricted, contract-aware functions rather than an
#' unrestricted general-purpose formula wrapper.
#'
#' @section Interpretation boundaries:
#' Behavioural measurements do not directly reveal emotion, stress,
#' cognition, comprehension, personality, diagnosis, deception, intention,
#' or other latent psychological states. Associations must not be described
#' as causal effects unless the design and estimand justify that language.
#'
#' @keywords internal
"_PACKAGE"

\name{multilevel-gaze-mediation}
\alias{multilevel-gaze-mediation}
\alias{create_mediation_prior_specification}
\alias{specify_multilevel_gaze_mediation}
\alias{fit_multilevel_gaze_mediation}
\alias{check_mediation_convergence}
\alias{prior_predictive_check_mediation}
\alias{posterior_predictive_check_mediation}
\alias{estimate_indirect_effect}
\alias{estimate_within_indirect_effect}
\alias{estimate_between_indirect_effect}
\alias{posterior_indirect_effect}
\alias{posterior_direct_effect}
\alias{posterior_total_effect}
\alias{summarise_multilevel_mediation}
\alias{plot_indirect_effect_distribution}
\alias{plot_mediation_posteriors}
\alias{plot_participant_mediation_effects}
\alias{compare_multilevel_mediation_models}
\alias{report_multilevel_gaze_mediation}
\alias{simulate_multilevel_gaze_mediation}
\alias{specify_multilevel_serial_gaze_mediation}
\alias{fit_multilevel_serial_gaze_mediation}
\alias{posterior_serial_indirect_effect}
\alias{specify_multilevel_moderated_gaze_mediation}
\alias{fit_multilevel_moderated_gaze_mediation}
\alias{posterior_conditional_indirect_effect}
\title{Bayesian Multilevel Gaze Mediation Workflows}
\description{
Contract-first Bayesian multilevel mediation workflows for trial-level
behavioural and gaze-derived data prepared under an explicit upstream
multilevel mediation contract. The API supports simple, serial, and
moderated mediation specifications, restricted Bayesian fitting,
diagnostics, posterior estimands, predictive checks, comparison guards,
simulation, plotting, and conservative reporting.
}
\usage{
create_mediation_prior_specification(
  intercept_sd = 2.5, coefficient_sd = 1, group_sd_scale = 1,
  residual_sd_scale = 1, dispersion_rate = 1,
  beta_precision_rate = 0.1
)

specify_multilevel_gaze_mediation(
  prepared, mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = "mediator_x", missingness_policy = "error"
)

fit_multilevel_gaze_mediation(
  prepared, mediator_family = "gaussian",
  outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = "mediator_x", missingness_policy = "error",
  chains = 4, iter = 2000, warmup = 1000,
  cores = min(chains, 2L), seed = 1, adapt_delta = 0.95,
  max_treedepth = 12, backend = c("cmdstanr", "rstan"),
  refresh = 0
)

check_mediation_convergence(
  fit, rhat_max = 1.01, ess_bulk_min = 400,
  divergences_max = 0L, treedepth_hits_max = 0L
)

prior_predictive_check_mediation(specification, ndraws = 200, seed = 1)

posterior_predictive_check_mediation(fit, ndraws = 200)

estimate_indirect_effect(
  fit, level = c("within", "between"), ...
)

estimate_within_indirect_effect(fit, ...)

estimate_between_indirect_effect(fit, ...)

posterior_indirect_effect(
  fit, level = c("within", "between"), probability = 0.95,
  require_convergence = TRUE
)

posterior_direct_effect(
  fit, level = c("within", "between"), probability = 0.95,
  require_convergence = TRUE
)

posterior_total_effect(
  fit, level = c("within", "between"), probability = 0.95,
  require_convergence = TRUE
)

summarise_multilevel_mediation(
  fit, probability = 0.95, require_convergence = TRUE
)

plot_indirect_effect_distribution(
  fit, level = c("within", "between"), probability = 0.95,
  require_convergence = TRUE
)

plot_mediation_posteriors(
  fit, probability = 0.95, require_convergence = TRUE
)

plot_participant_mediation_effects(fit)

compare_multilevel_mediation_models(...)

report_multilevel_gaze_mediation(
  fit, probability = 0.95, require_convergence = TRUE
)

simulate_multilevel_gaze_mediation(
  n_participants = 100L, trials_per_participant = 20L,
  a_within = 0.7, b_within = 0.8, cprime_within = 0.2,
  a_between = 0.2, b_between = 0.3, seed = 2026L
)

specify_multilevel_serial_gaze_mediation(
  prepared, mediator2_family = "gaussian",
  mediator_family = "gaussian", outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(), missingness_policy = "error"
)

fit_multilevel_serial_gaze_mediation(
  prepared, mediator2_family = "gaussian",
  mediator_family = "gaussian", outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(), missingness_policy = "error",
  chains = 4, iter = 2000, warmup = 1000,
  cores = min(chains, 2L), seed = 1, adapt_delta = 0.95,
  max_treedepth = 12, backend = c("cmdstanr", "rstan"),
  refresh = 0
)

posterior_serial_indirect_effect(
  fit, level = c("within", "between"), probability = 0.95,
  require_convergence = TRUE
)

specify_multilevel_moderated_gaze_mediation(
  prepared, moderation_path = c("a", "b"),
  moderator_component = c("within", "between"),
  mediator_family = "gaussian", outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(), missingness_policy = "error"
)

fit_multilevel_moderated_gaze_mediation(
  prepared, moderation_path = c("a", "b"),
  moderator_component = c("within", "between"),
  mediator_family = "gaussian", outcome_family = "bernoulli",
  priors = create_mediation_prior_specification(),
  random_slopes = character(), missingness_policy = "error",
  chains = 4, iter = 2000, warmup = 1000,
  cores = min(chains, 2L), seed = 1, adapt_delta = 0.95,
  max_treedepth = 12, backend = c("cmdstanr", "rstan"),
  refresh = 0
)

posterior_conditional_indirect_effect(
  fit, moderator_value, probability = 0.95,
  require_convergence = TRUE
)
}
\arguments{
  \item{prepared}{
    A canonical trial-level multilevel mediation preparation object,
    inheriting from \code{eye_multilevel_mediation_data}. Preparation,
    within/between decomposition, quality decisions, and exclusions are
    expected to be completed explicitly upstream.
  }
  \item{mediator_family}{
    Observation family for the primary mediator. Supported families are
    governed by the mediation specification contract.
  }
  \item{mediator2_family}{
    Observation family for the second mediator in a serial mediation
    specification.
  }
  \item{outcome_family}{
    Observation family for the outcome under the governed mediation
    contract.
  }
  \item{priors}{
    A mediation prior specification created by
    \code{create_mediation_prior_specification()}.
  }
  \item{random_slopes}{
    Character vector declaring supported participant-level random slopes.
    Requested slopes must be estimable from the observed data.
  }
  \item{missingness_policy}{
    Explicit missingness and eligibility policy. No observations are
    silently reclassified or imputed by these functions.
  }
  \item{intercept_sd, coefficient_sd, group_sd_scale, residual_sd_scale}{
    Positive scales controlling the corresponding prior distributions.
  }
  \item{dispersion_rate}{
    Positive rate used for governed dispersion priors when applicable.
  }
  \item{beta_precision_rate}{
    Positive rate used for the beta-family precision prior when applicable.
  }
  \item{chains}{Number of MCMC chains.}
  \item{iter}{Total iterations per chain.}
  \item{warmup}{Warmup iterations per chain.}
  \item{cores}{Number of processor cores used for fitting.}
  \item{seed}{Non-negative random seed used for deterministic simulation or fitting.}
  \item{adapt_delta}{Target acceptance probability supplied to the Bayesian sampler.}
  \item{max_treedepth}{Maximum sampler tree depth.}
  \item{backend}{Restricted \code{brms} backend, either \code{"cmdstanr"} or \code{"rstan"}.}
  \item{refresh}{Sampler progress refresh interval.}
  \item{fit}{
    A fitted \code{gp3bayes_multilevel_mediation_fit} object produced by
    one of the governed mediation fitting functions.
  }
  \item{rhat_max}{Maximum accepted R-hat threshold for the convergence audit.}
  \item{ess_bulk_min}{Minimum accepted bulk effective sample size.}
  \item{divergences_max}{Maximum accepted number of divergent transitions.}
  \item{treedepth_hits_max}{Maximum accepted number of maximum-treedepth hits.}
  \item{specification}{A governed multilevel mediation specification object.}
  \item{ndraws}{Number of predictive draws requested for the predictive check.}
  \item{level}{Whether the requested estimand is within- or between-participant.}
  \item{probability}{Posterior interval probability.}
  \item{require_convergence}{
    If \code{TRUE}, posterior estimands that require an acceptable
    convergence audit are not returned when critical diagnostics fail.
  }
  \item{...}{
    For model comparison, fitted mediation objects to compare. For the
    convenience indirect-effect wrappers, additional arguments forwarded
    to the corresponding posterior estimand function.
  }
  \item{n_participants}{Number of participants generated by the simulator.}
  \item{trials_per_participant}{Number of simulated trials per participant.}
  \item{a_within, b_within, cprime_within}{
    Declared within-participant simulation path coefficients.
  }
  \item{a_between, b_between}{
    Declared between-participant simulation path coefficients.
  }
  \item{moderation_path}{
    Path on which moderation is declared, restricted to the supported
    mediation paths.
  }
  \item{moderator_component}{
    Whether moderation is represented by the within- or between-participant
    component of the declared moderator.
  }
  \item{moderator_value}{
    Moderator value at which the conditional indirect effect is evaluated.
  }
}
\details{
These functions implement a contract-first Bayesian mediation layer for
repeated-measures data. The package does not silently perform the upstream
within/between decomposition, resolve quality exclusions, impute missing
observations, or broaden the requested random-effects structure.

Simple mediation separates within- and between-participant paths when
supported by the prepared data. Serial and moderated specifications are
available through explicitly named APIs rather than automatic model search.

Posterior products involving non-Gaussian mediator or outcome models are
defined on the model's linear-predictor scale unless a function explicitly
states otherwise. They must not be interpreted automatically as probability-
scale, risk-ratio, causal, physiological, or cognitive effects.

\code{compare_multilevel_mediation_models()} requires competing fitted
models to contain the same mediator/outcome observations in the same
participant-trial order. Fits based on different missingness or exclusion
sets are rejected before PSIS-LOO comparison.

Convergence, predictive checks, posterior intervals, and sensitivity
summaries remain separate pieces of evidence. None automatically establish
model adequacy, robustness, causality, or substantive importance.
}
\value{
Depending on the function, a mediation prior specification, model
specification, fitted model wrapper, convergence audit, predictive-check
object, posterior estimand summary, simulation object, publication-oriented
summary, report, model comparison, or plot is returned. Returned objects
retain the model specification and relevant provenance where applicable.
}
\seealso{
\code{\link{estimate_scr_responsivity_bayes}}
}
\examples{
\dontrun{
# Trial-level data must first be prepared under the upstream
# multilevel mediation contract.
priors <- create_mediation_prior_specification()
}
}

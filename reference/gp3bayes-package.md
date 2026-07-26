# gp3bayes: Contract-First Bayesian Workflows for Hierarchical Behavioural Data

`gp3bayes` provides package-neutral infrastructure for transparent,
contract-first Bayesian workflows for repeated-measures and hierarchical
behavioural data. It implements approved Bernoulli-logit and positive
lognormal duration workflows with deterministic simulation, recorded
preparation, inspectable priors, restricted optional full-MCMC fitting,
sampling diagnostics, posterior predictive checks, prior sensitivity,
simulation-based recovery, and conservative structured reporting.
Fitting or passing a numerical threshold does not by itself establish
convergence, posterior adequacy, causal identification, or validity.

## Approved model families

The approved model-family scope is restricted to:

- hierarchical Bernoulli-logit models for binary trial-level outcomes;

- hierarchical lognormal models for strictly positive uncensored
  durations.

Additional outcome families require separate methodological approval.

## Backend policy

Core validation, contract, simulation, preparation, transformation,
specification, and prior-predictive functionality remains usable without
a Bayesian backend. Restricted full-MCMC fitting uses the `brms`
interface. The original
[`fit_binary_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model.md)
and
[`fit_duration_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_duration_model.md)
interfaces retain the fixed `rstan` route, while the backend-portable
[`fit_binary_model_backend()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model_backend.md)
and
[`fit_duration_model_backend()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_duration_model_backend.md)
interfaces support either `rstan` or `cmdstanr`. Model families,
formulas, priors, and algorithms remain contract-restricted.

## Interpretation boundaries

Behavioural measurements do not directly reveal emotion, stress,
cognition, comprehension, personality, diagnosis, deception, intention,
or other latent psychological states. Associations must not be described
as causal effects unless the design and estimand justify that language.

## See also

Useful links:

- <https://stefanosbalaskas.github.io/gp3bayes/>

- <https://github.com/stefanosbalaskas/gp3bayes>

- Report bugs at <https://github.com/stefanosbalaskas/gp3bayes/issues>

## Author

**Maintainer**: Stefanos Balaskas <s.balaskas@ac.upatras.gr>
([ORCID](https://orcid.org/0000-0003-2444-9796)) \[copyright holder\]

Authors:

- Stefanos Balaskas <s.balaskas@ac.upatras.gr>
  ([ORCID](https://orcid.org/0000-0003-2444-9796)) \[copyright holder\]

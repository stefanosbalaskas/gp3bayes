# gp3bayes

[![DOI](https://zenodo.org/badge/1305351994.svg)](https://doi.org/10.5281/zenodo.21518698)

`gp3bayes` is an independent R package for transparent, contract-first
Bayesian workflows for repeated-measures and hierarchical behavioural
data.

## Scope

The current development version is `gp3bayes` 0.2.0.9001.

The package provides:

- explicit model contracts and standard or strict model-readiness
  audits;
- deterministic hierarchical binary and positive-duration simulation
  with stored truth;
- recorded outcome, condition, unit, missing-value, coding, and scaling
  decisions;
- reusable transformation recipes with replay, inversion, and
  validation;
- inspectable backend-independent prior specifications and prior
  predictive checks;
- restricted Bernoulli-logit and lognormal model construction;
- optional full-MCMC fitting through `brms` with either `rstan` or
  `cmdstanr`;
- R-hat, bulk/tail ESS, divergence, treedepth, and energy diagnostics;
- family-specific posterior summaries and detailed posterior predictive
  checks;
- design-standardised binary probability contrasts and duration median,
  ratio, and predictive-quantile estimands;
- prior-scale, power-scaling, structural, deletion, contrast-coding,
  predictor-scaling, and duration-unit sensitivity workflows;
- PSIS-LOO diagnostics, influence assessment, model comparison, model
  weights, and optional exact K-fold validation;
- fixed-effects separation screening and simulation-based calibration;
- specification traceability and conservative structured reporting
  without automatic adequacy, robustness, exclusion, or model-selection
  claims.

The approved model-family scope remains restricted to:

1.  hierarchical Bernoulli-logit models for binary trial-level outcomes;
2.  hierarchical lognormal models for strictly positive uncensored
    durations.

Core contract, validation, simulation, preparation, transformation,
specification, and prior-predictive functionality does not require
Gazepoint hardware, Gazepoint exports, `gp3tools`, proprietary software,
private data, or a Bayesian backend. Full-MCMC fitting requires `brms`
and one supported sampling backend: `rstan` or `cmdstanr`.

## Model contracts

[`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md)
records the approved methodological specification and neutral column
mappings for one initial model family. Creating a contract does not
validate data, fit a model, or establish model adequacy.

``` r

binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "stimulus_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

binary_contract
```

``` R
## <gp3bayes_model_contract>
##   Family: binary
##   Likelihood: Bernoulli
##   Link: logit
##   Outcome: selected
##   Participant: participant_id
##   Item: stimulus_id
##   Condition: condition
##   Random slope requested: FALSE
##   Fitting performed: FALSE
```

## Readiness audits

[`audit_model_readiness()`](https://stefanosbalaskas.github.io/gp3bayes/reference/audit_model_readiness.md)
evaluates observable data requirements before formula construction or
model fitting. Failures block progression, whereas warnings identify
structures requiring review.

``` r

binary_data <- data.frame(
  participant_id = rep(c("p1", "p2"), each = 4),
  stimulus_id = rep(paste0("s", 1:4), times = 2),
  trial_id = rep(1:4, times = 2),
  condition = rep(c("control", "treatment"), times = 4),
  selected = c(0, 1, 0, 1, 1, 0, 1, 0)
)

readiness_audit <- audit_model_readiness(
  binary_data,
  binary_contract
)

readiness_audit
```

``` R
## <gp3bayes_readiness_audit>
##   Family: binary
##   Rows: 8
##   Status: ready
##   Ready: TRUE
##   Checks: 18 passed, 0 warnings, 0 failures
```

## Model specifications

[`build_model_formula()`](https://stefanosbalaskas.github.io/gp3bayes/reference/build_model_formula.md)
translates the approved contract into an R formula, while
[`create_prior_specification()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_prior_specification.md)
records family-appropriate priors without creating backend-specific
objects. A ready audit, formula, contract, and validated priors can then
be combined into one inspectable model specification.

``` r

binary_priors <- create_prior_specification(
  binary_contract,
  baseline = 0.5
)

binary_specification <- create_model_specification(
  binary_contract,
  readiness_audit,
  binary_priors
)

binary_specification
```

``` R
## <gp3bayes_model_specification>
##   Family: binary
##   Formula: selected ~ condition + (1 | participant_id) + (1 | stimulus_id)
##   Readiness status: ready
##   Readiness warnings: 0
##   Prior classes: Intercept, b, sd
##   Backend: none
##   Fit performed: FALSE
```

## Hierarchical binary workflow foundation

The backend-independent binary workflow can simulate known hierarchical
data-generating processes, prepare neutral long-format data, construct a
restricted model specification, and evaluate prior predictive
plausibility. No model is fitted and no posterior draws are produced.

``` r

binary_simulation <- simulate_hierarchical_binary_data(
  n_participants = 12,
  trials_per_participant = 8,
  n_items = 6,
  random_slope_sd = 0,
  seed = 2026
)

binary_workflow_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = "trial_covariate"
)

binary_prepared <- prepare_hierarchical_binary_data(
  binary_simulation$data,
  binary_workflow_contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = "trial_covariate"
)

binary_workflow_specification <- specify_binary_model(
  binary_prepared,
  baseline = 0.35
)

binary_prior_check <- check_binary_prior_predictive(
  binary_workflow_specification,
  draws = 100,
  seed = 2027
)

binary_prior_check
```

``` R
## <gp3bayes_binary_prior_predictive_check>
##   Adequate: TRUE
##   Draws: 100
##   Failed checks: 0
##   Backend: none
##   Fit performed: FALSE
```

## Restricted binary model fitting

[`translate_binary_model_to_brms()`](https://stefanosbalaskas.github.io/gp3bayes/reference/translate_binary_model_to_brms.md)
converts an approved package specification into a restricted
Bernoulli-logit `brms` representation without compiling or fitting a
model.

[`fit_binary_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model.md)
retains the original fixed `rstan` fitting route. For backend-portable
full-MCMC fitting,
[`fit_binary_model_backend()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model_backend.md)
accepts either `backend = "rstan"` or `backend = "cmdstanr"` while
preserving the same approved family, formula, priors, and sampling
contract.

Neither interface accepts unrestricted formulas, arbitrary model
families, alternative inference algorithms, user-supplied Stan programs,
or arbitrary backend arguments.

``` r

if (requireNamespace("brms", quietly = TRUE)) {
  backend_specification <- translate_binary_model_to_brms(
    binary_workflow_specification
  )

  backend_specification
}
```

A returned fit does not by itself establish convergence, posterior
adequacy, causal identification, or substantive validity. Those
assessments require separate diagnostic and reporting gates.

## Binary posterior validation

Approved binary fits can be assessed with conservative numerical
sampling diagnostics, posterior summaries, posterior predictive checks,
prior-scale sensitivity, simulation-based recovery, and structured
Markdown reports. A threshold pass is not an automatic convergence or
posterior-adequacy claim.

``` r

diagnostics <- diagnose_binary_fit(binary_fit)
posterior <- summarise_binary_posterior(binary_fit)
predictive <- check_binary_posterior_predictive(binary_fit)
```

## Hierarchical lognormal duration workflow

The duration workflow supports strictly positive, finite, uncensored
durations with an explicit recorded unit. It provides deterministic
simulation, preparation, inspectable priors, prior predictive checks,
and restricted optional full-MCMC fitting through `brms` with either
`rstan` or `cmdstanr`.

``` r

duration_simulation <- simulate_hierarchical_duration_data(seed = 2026)
duration_contract <- create_model_contract(
  family = "duration",
  outcome_col = "duration",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit = "milliseconds"
)
duration_prepared <- prepare_hierarchical_duration_data(
  duration_simulation$data,
  duration_contract,
  condition_levels = c("control", "treatment")
)
duration_specification <- specify_duration_model(
  duration_prepared,
  baseline = 500
)
```

## Duration posterior validation

Approved lognormal duration fits support the same conservative
diagnostic contract as binary fits, together with positive-scale
posterior predictive checks, prior sensitivity, simulation-based
recovery, and structured reports. Exponentiated population coefficients
are conditional median ratios, not automatically causal effects.

``` r

duration_diagnostics <- diagnose_duration_fit(duration_fit)
duration_posterior <- summarise_duration_posterior(duration_fit)
duration_predictive <- check_duration_posterior_predictive(duration_fit)
```

## Citation

Citation metadata are provided in both `CITATION.cff` and
`inst/CITATION`. After installing the package, obtain the R-formatted
citation for the installed version with:

``` r

citation("gp3bayes")
```

The repository currently represents development version **0.2.0.9001**.

For archived software citation:

- Latest archived stable release: `gp3bayes` 0.1.0 —
  [`10.5281/zenodo.21518699`](https://doi.org/10.5281/zenodo.21518699)
- Concept DOI for all releases —
  [`10.5281/zenodo.21518698`](https://doi.org/10.5281/zenodo.21518698)
  \## Development status

`gp3bayes` 0.2.0.9001 is the current development version.

The development API provides contract-first Bernoulli-logit and
lognormal-duration workflows; backend-portable full-MCMC fitting through
`brms` with either `rstan` or `cmdstanr`; advanced sensitivity,
PSIS-LOO, model-weighting, separation-screening, and simulation-based
calibration workflows; strict specification closure; transformation
replay; explicit posterior estimands; detailed posterior predictive
checks; and optional exact K-fold validation.

The latest archived stable release remains `gp3bayes` 0.1.0. \##
Interpretation boundaries

Behavioural, gaze, pupil, and physiological measurements do not directly
reveal emotion, stress, cognition, comprehension, personality,
diagnosis, deception, intention, or other latent psychological states.

Associations must not be described as causal effects unless the study
design and target estimand justify causal interpretation.

## Advanced optional workflows

The 0.2.0.9001 development API includes conservative optional adapters
for power-scaled sensitivity through `priorsense`, PSIS-LOO diagnostics
and model weights through `loo`, fixed-effects separation screening
through `detectseparation`, simulation-based calibration through `SBC`,
and full-MCMC backend selection between `rstan` and `cmdstanr`.

Dedicated binary and duration pathology generators make documented
failure cases directly reproducible.

These extensions do not introduce unrestricted formulas, arbitrary model
families, automatic model selection, automatic exclusion, or automatic
adequacy claims. \## Licence

`gp3bayes` is released under the MIT License.

## Specification closure

The development API now closes the remaining contract-level requirements
with strict readiness audits, exact transformation replay, first-class
probability/median/tail estimands, governed structural and deletion
sensitivity, duration-unit invariance, detailed posterior predictive
checks, and optional exact K-fold validation.

These functions remain inside the two approved Bernoulli-logit and
positive uncensored lognormal-duration contracts. They do not add
arbitrary formulas, likelihoods, automatic model selection, automatic
exclusions, or causal claims.

``` r

audit_model_readiness_strict(data, contract)
create_transformation_recipe(prepared)
estimate_standardized_probability_contrast(fit)
estimate_standardized_duration_estimands(fit)
check_binary_ppc_details(fit)
check_duration_ppc_details(fit)
gp3bayes_specification_traceability()
```

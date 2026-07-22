# gp3bayes

`gp3bayes` is an independent R package under development for
transparent, contract-first Bayesian workflows for repeated-measures and
hierarchical behavioural data.

## Scope

The package currently provides:

- explicit model contracts;
- model-readiness audits;
- deterministic hierarchical binary simulation with recorded truth;
- explicit binary-outcome and condition transformations;
- inspectable backend-independent prior specifications;
- restricted hierarchical formula construction;
- structured prior-predictive plausibility checks;
- restricted translation of approved binary specifications to `brms`;
- optional full-MCMC binary fitting through the fixed `brms` and `rstan`
  route.

The initial development scope is restricted to:

1.  hierarchical Bernoulli-logit models for binary trial-level outcomes;
2.  hierarchical lognormal models for strictly positive uncensored
    durations.

Core contract, validation, simulation, preparation, specification, and
prior-predictive functionality does not require Gazepoint hardware,
Gazepoint exports, `gp3tools`, proprietary software, private data, or a
Bayesian backend. Binary fitting requires the optional `brms` and
`rstan` packages.

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
converts an approved package specification into a fixed Bernoulli-logit
`brms` representation without compiling or fitting a model.
[`fit_binary_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/fit_binary_model.md)
optionally runs full MCMC sampling through the fixed `brms` and `rstan`
route. Neither function accepts an unrestricted formula, family,
backend, algorithm, Stan extension, or arbitrary backend arguments.

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

## Citation

Citation metadata are provided in both `CITATION.cff` and
`inst/CITATION`. After installing the package, obtain the current
R-formatted citation with:

``` r

citation("gp3bayes")
```

## Development status

`gp3bayes` is currently at development version `0.0.0.9000`.

No public model-fitting API is available yet. The current development
stage establishes the standalone package structure, approved
methodological scope, data-contract principles, and validation
infrastructure.

## Interpretation boundaries

Behavioural, gaze, pupil, and physiological measurements do not directly
reveal emotion, stress, cognition, comprehension, personality,
diagnosis, deception, intention, or other latent psychological states.

Associations must not be described as causal effects unless the study
design and target estimand justify causal interpretation.

## Licence

`gp3bayes` is released under the MIT License.

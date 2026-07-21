# gp3bayes 0.0.0.9000

* Created the independent `gp3bayes` package scaffold.
* Defined the initial scope as contract-first Bayesian workflows for
  hierarchical behavioural data.
* Restricted initial development to hierarchical Bernoulli-logit and
  hierarchical lognormal-duration model families.
* Added package-level documentation and explicit interpretation boundaries.
* Added the initial deterministic scope test using testthat edition 3.
* Added the MIT licence.
* Added standard GitHub Actions workflows for cross-platform R CMD check
  and pkgdown deployment.
* Added canonical repository, issue-tracker, and pkgdown website metadata.
* Added `create_model_contract()` for the two approved initial model
  families with neutral column mappings and explicit methodological
  specifications.
* Added a concise `gp3bayes_model_contract` print method and deterministic
  validation tests.
* Added `audit_model_readiness()` for backend-independent assessment of
  outcome validity, declared columns, missingness, repeated measurements,
  item and trial structure, predictors, interactions, time terms, and
  requested participant-level random slopes.
* Added structured `gp3bayes_readiness_audit` results with explicit pass,
  warning, and failure statuses and a concise print method.
* Added `build_model_formula()` for deterministic, backend-independent
  construction of approved fixed-effects, interaction, participant, item,
  time, and optional participant-level random-slope structures.
* Added `create_prior_specification()` and
  `validate_prior_specification()` for explicit binary-logit and
  lognormal-duration prior records without creating executable backend
  objects.
* Added `create_model_specification()` to combine a model contract,
  successful readiness audit, approved formula, and validated priors into
  one inspectable backend-independent specification.
* Added concise print methods and deterministic validation tests for
  formulas, priors, compatibility checks, and complete specifications.
* Added `simulate_hierarchical_binary_data()` for deterministic
  hierarchical Bernoulli-logit simulation with participant effects,
  optional crossed item effects, optional participant condition slopes,
  controlled imbalance, and a stored true-parameter record.
* Added `prepare_hierarchical_binary_data()` for explicit binary-outcome
  mapping, condition coding, recorded predictor scaling, missing-data
  decisions, readiness auditing, and fixed-effects matrix construction.
* Added `specify_binary_model()` to combine prepared data with the
  approved binary contract, restricted hierarchical formula, and
  validated backend-independent prior specification.
* Added `check_binary_prior_predictive()` for deterministic simulation
  of family-specific prior predictions and structured plausibility checks
  without fitting a model or requiring a Bayesian backend.
* Added concise print methods, generated documentation, and 89 focused
  tests for the backend-independent binary workflow foundation.
* Added repository and installed-package citation metadata through
  `CITATION.cff` and `inst/CITATION`.
* Refined the package description to match the currently implemented
  backend-independent contract, readiness, simulation, preparation,
  specification, and prior-predictive functionality.

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

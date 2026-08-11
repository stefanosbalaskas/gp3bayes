# gp3bayes 0.2.0

## 0.2.0 stabilization program

* Adds a stable family-neutral workflow API while retaining the existing
  binary- and duration-specific interfaces.
* Adds analysis manifests, data/specification fingerprints, explicit
  manifest freezing/comparison, and reproducibility reports.
* Adds pre-fit missingness, fixed-effect design, random-effect support,
  and combined design-support audits without automatic data/model changes.
* Adds declarative unified sensitivity suites and evidence inventories
  without aggregate robustness, adequacy, exclusion, or selection claims.
* Adds backend environment validation, MCSE-aware rstan/cmdstanr posterior
  parity auditing, and serialized gp3bayes object-schema contracts.
* Forward-ports CRAN 0.1.1 compliance safeguards: two-core automatic
  defaults, explicit report paths, temporary vignette outputs, and safe
  seed handling without direct global-environment modification.
* Adds five focused stabilization articles plus an integrated synthetic
  0.2.0 release case study, tests, and release smoke/audit scripts.


* Aligned DESCRIPTION, README, citation metadata, package-level help,
  backend-installation guidance, CRAN comments, and pkgdown deployment metadata
  with the complete 0.2.0 API and dual `rstan`/`cmdstanr`
  backend support.

## Specification closure

* Adds strict readiness checks for overall condition imbalance, binary group outcome variation, identifier-like numeric predictors, fixed-effect rank, duration extremes, declared duration ranges, censoring signals, and optional separation screening.
* Adds reusable transformation recipes with forward replay, inversion, and exact replay validation for retained rows.
* Adds first-class design-standardised binary probability contrasts and duration median, ratio, and predictive-quantile estimands.
* Adds governed structural, group-deletion, contrast-coding, predictor-scaling, and duration-unit sensitivity workflows without automatic model selection or exclusion.
* Adds detailed family-specific posterior predictive checks and plotting helpers.
* Adds a governed exact K-fold adapter through `brms::kfold()` as an optional predictive-validation fallback/complement to PSIS-LOO.
* Adds an auditable specification-traceability matrix, examples, smoke tests, and three integrated articles.

* Added optional power-scaling sensitivity integration through `priorsense`.
* Added conservative PSIS-LOO diagnostics, influence inspection, model
  comparison, and stacking or pseudo-BMA weights through `loo`.
* Added fixed-effects separation screening through `detectseparation`.
* Added simulation-based calibration plans and plots through `SBC`.
* Added restricted full-MCMC backend selection between `rstan` and
  `cmdstanr`.
* Added coefficient-specific interaction-prior defaults for binary and
  duration contracts.
* Added dedicated binary and duration pathology generators, evaluations,
  plots, tests, examples, smoke tests, and three integrated articles.

# gp3bayes 0.1.1

* Published `gp3bayes` 0.1.1 on CRAN after addressing CRAN review feedback.
* Added Zenodo DOI documentation and current release-status wording.
* Added explicit copyright-holder metadata for the initial CRAN submission.

# gp3bayes 0.1.0

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
* Added restricted binary model translation from approved package
  specifications to `brms` Bernoulli-logit formulas and priors.
* Added optional full-MCMC fitting through the fixed `brms` and `rstan`
  sampling route without unrestricted formulas or backend arguments.
* Added conservative fit metadata that records sampling settings while
  explicitly withholding convergence and posterior-adequacy claims.
* Added conservative binary posterior diagnostics covering R-hat,
  bulk and tail ESS, divergences, maximum-treedepth saturation, and
  chain-level energy diagnostics.
* Added posterior summaries, binary posterior predictive checks,
  prior-scale sensitivity, simulation-based recovery, diagnostic plots,
  and structured Markdown model reports.
* Diagnostic, predictive, sensitivity, and recovery statuses never create
  automatic convergence, adequacy, robustness, or validation claims.
* Added the complete hierarchical lognormal duration workflow for
  strictly positive finite uncensored outcomes.
* Added deterministic duration simulation, explicit unit conversion and
  preparation, model specification, prior predictive checks, restricted
  `brms` translation, and full MCMC fitting through `rstan`.
* Zero, negative, censored, truncated, shifted, survival, Gamma, Weibull,
  and mixture outcomes remain outside the approved duration contract.
* Added conservative duration posterior diagnostics, posterior summaries
  with median-ratio interpretation, posterior predictive checks,
  prior-scale sensitivity, simulation-based recovery, and structured
  Markdown reports.
* Duration validation statuses remain separate from automatic convergence,
  adequacy, robustness, causal, or substantive claims.
* Added integrated end-to-end binary and duration vignettes.
* Added dedicated articles for sampling diagnostics, prior sensitivity and
  recovery, and optional backend installation.
* Added a repository-only audit covering exports, Rd aliases, pkgdown
  reference topics, articles, built pages, and optional dependencies.
* Aligned DESCRIPTION, citation metadata, README, package documentation, and
  the curated pkgdown reference and article indices with the complete scope.

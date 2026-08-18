# CRAN submission comments

## Submission

This is an update to the CRAN package `gp3bayes`, from version 0.1.1 to version 0.5.0.

## Release scope

`gp3bayes` 0.5.0 extends the package's contract-first Bayesian workflow with governed advanced pupillometry functionality while preserving the existing hierarchical binary and positive lognormal-duration workflows.

The release exposes 458 public functions and 230 S3 registrations.

The package does not automatically make causal, cognitive-state, model-adequacy, robustness, exclusion, or model-selection claims.

## Validation


* Final exact `gp3bayes_0.5.0.tar.gz` `R CMD check --as-cran`:
  0 errors, 0 warnings, 0 notes.
* Exact archive bytes: `916189`.
* Exact archive MD5: `3571e36aaeac4ecb7ebb0094a2f514cf`.
* Exact archive SHA256: `d6c9ac94ea1d9ba8fd7377fb350c3201b16fbdf91320c16d709192a33258d400`.
* Windows 11 x64, R 4.6.1.
* Full backend-free package test suite: PASS.
* Frozen public API/signature contract: PASS.
* Public exports: 458.
* S3 registrations: 230.
* Rd files: 465.
* Vignette sources: 59.
* No Stan compilation or sampling is required by the release check.

## Optional repositories

* `cmdstanr` is an optional Suggests dependency available from
  `https://stan-dev.r-universe.dev`.
* `SBC` is an optional Suggests dependency available from
  `https://r-multiverse.r-universe.dev`.
* Neither is required for installation or the backend-independent core.

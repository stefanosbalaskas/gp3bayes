## gp3bayes 0.2.0 release preparation

* Current CRAN release: `gp3bayes` 0.1.1 (published 2026-08-09).
* This source prepares `gp3bayes` 0.2.0 for release.
* The historical 0.1.1 CRAN resubmission remains isolated; its
  accepted compliance safeguards were manually forward-ported.

## Completed stabilization validation

* Windows 11 x64, R 4.6.1.
* Full test suite: 635 passed, 0 failed, 0 warnings, 0 skipped.
* `pkgdown::check_pkgdown()`: no problems.
* Complete pkgdown site build: passed.
* Six stabilization articles rendered successfully.
* `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes.
* rstan compiler validation: passed.
* cmdstanr/CmdStan toolchain validation: passed.
* Binary rstan/cmdstanr parity: passed with 0 review parameters.
* Duration rstan/cmdstanr parity: passed with 0 review parameters.
* Sampling diagnostics passed for binary and duration models through
  both supported backends.

## Final release validation

* Exact `gp3bayes_0.2.0.tar.gz` build and tarball-level CRAN check
  will be recorded after the release source is frozen.


## Optional package repositories

* `cmdstanr` is an optional suggested package available from
  `https://stan-dev.r-universe.dev`.
* `SBC` is an optional suggested package available from
  `https://r-multiverse.r-universe.dev`.
* Both are used conditionally; neither is required to install or
  use the backend-independent core package.

## Exact 0.2.0 source-archive validation

* Exact source archive: `gp3bayes_0.2.0.tar.gz`.
* `R CMD build`: completed with no build warnings.
* Frozen schema artifacts use RDS serialization version 2.
* Exact tarball `R CMD check --as-cran --no-manual`:
  0 errors, 0 warnings, 1 CRAN incoming-feasibility NOTE.
* The NOTE concerns optional suggested packages outside the
  mainstream repositories.
* `cmdstanr` resolves through `https://stan-dev.r-universe.dev`.
* `SBC` resolves through `https://r-multiverse.r-universe.dev`.
* Neither optional package is required for installation or the
  backend-independent core workflow.

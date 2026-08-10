# gp3bayes 0.2.0 release-hardening checklist

## Release synchronization

- [x] Confirm CRAN serves 0.1.1.
- [x] Confirm annotated `v0.1.1` tag points to accepted release-correction commit `5638772`.
- [x] Keep `cran/0.1.1-resubmission` separate from `master`.
- [x] Confirm development branch started from current `master` and version remains `0.2.0.9001` until release preparation.
- [ ] Do not invent a 0.1.1 Zenodo version DOI; update version-specific archival metadata only after the DOI exists.

## CRAN feedback forward-port

- [x] No `\\dontrun{}` remains in executable examples.
- [x] Slow/optional backend example uses `\\donttest{}`.
- [x] Automatic core default is capped at two.
- [x] Examples/vignettes/tests never request more than two cores.
- [x] Report writers require explicit paths.
- [x] Vignette writes use temporary files and clean them.
- [x] No `.GlobalEnv`, `globalenv()`, or `<<-` remains in package R code.
- [x] Seed preservation uses `withr::with_seed()`.

## Stable 0.2.0 API

- [x] Unified workflow wrappers documented and tested.
- [x] Analysis manifest layer documented and tested.
- [x] Pre-fit design-support audits documented and tested.
- [x] Sensitivity suite and evidence inventory documented and tested.
- [x] Backend capability/environment/parity layer documented and tested.
- [x] Object-schema compatibility layer documented and tested.
- [x] All new report/file functions write only to explicit user paths or `tempfile()`.

## Package validation

- [x] `source("tools/audit-stabilization-0.2.0.R")` passes.
- [x] `source("tools/smoke-stabilization-0.2.0.R")` passes.
- [x] `devtools::test()` passes completely.
- [x] Each new vignette renders independently.
- [x] `pkgdown::check_pkgdown()` reports no problems.
- [x] `pkgdown::build_site(preview = FALSE, new_process = TRUE)` succeeds.
- [x] `devtools::check(args = "--as-cran")` has 0 errors, 0 warnings, 0 unexpected notes.
- [ ] `R CMD check --as-cran` on the built tarball is clean apart from any documented incoming NOTE.

## Real backends

- [x] `validate_backend_environment("rstan", compile_test = TRUE, strict = TRUE)` passes.
- [x] `validate_backend_environment("cmdstanr", compile_test = TRUE, strict = TRUE)` passes.
- [x] Existing real-backend smoke scripts pass.
- [x] `tools/smoke-backend-parity-0.2.0.R` passes or any review differences are scientifically inspected and documented.
- [x] `tools/smoke-duration-backend-parity-0.2.0.R` passes or any review differences are scientifically inspected and documented.
- [x] Binary real-backend fit tested through both backends.
- [x] Duration real-backend fit tested through both backends.

## Release artifacts

- [x] README describes CRAN 0.1.1 correctly and development 0.2.0.9001 correctly.
- [x] NEWS documents the stabilization milestone.
- [x] pkgdown reference index includes new function groups.
- [x] Six stabilization articles are visible in the Articles navbar.
- [x] Public pkgdown is deployed immediately after merge.
- [x] Object schemas intended as release compatibility baselines are captured only after APIs are frozen.
- [x] Bump development version to 0.2.0 only on the release-preparation branch.
- [ ] Build and inspect final `gp3bayes_0.2.0.tar.gz`.
- [ ] Create GitHub release and Zenodo archive after validation.

## Validated 0.2.0 stabilization environment

- Date: 2026-08-10.
- Windows 11 x64, R 4.6.1.
- gp3bayes development version: 0.2.0.9001.
- brms: 2.23.0.
- rstan: 2.32.7.
- StanHeaders: 2.32.10.
- cmdstanr: 0.9.0.
- CmdStan: 2.39.0.
- posterior: 1.7.0.
- loo: 2.10.0.
- Full test suite: 635 passed, 0 failed, 0 warnings, 0 skipped.
- R CMD check --as-cran: 0 errors, 0 warnings, 0 notes.
- rstan compiler smoke test: passed.
- cmdstanr/CmdStan toolchain smoke test: passed.
- Binary rstan/cmdstanr parity: passed with 0 review parameters.
- Duration rstan/cmdstanr parity: passed with 0 review parameters.
- Binary rstan sampling diagnostics: passed.
- Binary cmdstanr sampling diagnostics: passed.
- Duration rstan sampling diagnostics: passed.
- Duration cmdstanr sampling diagnostics: passed.
- Identical draws across backends are not expected or required.

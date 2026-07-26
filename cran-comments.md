## Development version

Current development version: gp3bayes 0.2.0.9001.

The latest archived stable release is gp3bayes 0.1.0.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Windows 11 x64, R 4.6.1
* GitHub Actions: Windows, macOS, and Ubuntu with R release
* GitHub Actions: Ubuntu with R-devel and R oldrel-1

## Additional validation

* Full testthat suite: 514 tests passed.
* `pkgdown::check_pkgdown()`: no problems.
* Specification-closure backend-independent smoke test: passed.
* Real-backend smoke test: passed with both `rstan` and `cmdstanr`.
* Exact K-fold smoke validation: passed with both `rstan` and `cmdstanr`.
* `R CMD check --as-cran --run-donttest`: 0 errors, 0 warnings, 0 notes.

This file records development validation and should be refreshed again before a
future CRAN submission.

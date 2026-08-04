## Resubmission

This is a resubmission of gp3bayes 0.1.1.

In response to CRAN feedback:

* replaced the `\dontrun{}` fitting example with a guarded
  `\donttest{}` example;
* ensured examples, tests, and vignettes use no more than two cores;
* removed default report-output paths and required callers to provide
  an explicit path;
* changed vignette report outputs to temporary files and removed them
  after use;
* removed direct `.GlobalEnv` modification and package-code
  superassignment.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Test environment

* Windows 11 x64, R 4.6.1.

## Additional validation

* Static CRAN compliance audit passed.
* `devtools::test()`: 414 passed, 0 failed, 0 warnings, 0 skipped.
* Both modified vignettes rendered successfully using temporary
  output locations.
* `pkgdown::check_pkgdown()`: no problems.
* `R CMD check --as-cran --no-manual` completed with 0 errors,
  0 warnings, and 1 expected NOTE for a new submission.
* The guarded `\donttest{}` full-MCMC example was executed
  successfully during the check.

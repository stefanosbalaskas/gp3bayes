# Validate a Bayesian Backend Environment

Checks whether the packages and external runtime required by one
approved gp3bayes backend are available. With `compile_test = TRUE`, an
optional minimal compiler smoke test is performed. The smoke test does
not fit a statistical model and writes no persistent files.

## Usage

``` r
validate_backend_environment(
  backend = c("rstan", "cmdstanr"),
  compile_test = FALSE,
  strict = FALSE
)
```

## Arguments

- backend:

  Either `"rstan"` or `"cmdstanr"`.

- compile_test:

  Whether to run an optional compiler smoke test.

- strict:

  Whether an unavailable backend should raise an error.

## Value

A `gp3bayes_backend_environment` object.

## Examples

``` r
validate_backend_environment("rstan")
#> <gp3bayes_backend_environment>
#>   Backend: rstan
#>   Status: ready
#>   Compiler smoke test requested: FALSE
#>                check       status                             detail
#>         brms_package         pass                             2.23.0
#>      backend_package         pass                             2.32.7
#>     external_runtime         pass managed by rstan package/toolchain
#>  compiler_smoke_test not_assessed                      not requested
validate_backend_environment("cmdstanr")
#> <gp3bayes_backend_environment>
#>   Backend: cmdstanr
#>   Status: fail
#>   Compiler smoke test requested: FALSE
#>                check       status        detail
#>         brms_package         pass        2.23.0
#>      backend_package         pass         0.9.0
#>     external_runtime         fail          <NA>
#>  compiler_smoke_test not_assessed not requested
```

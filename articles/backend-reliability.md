# Backend Reliability, Parity and Object Schemas

## Two interchangeable implementation backends, one modeling contract

The approved full-MCMC interface remains `brms` with either `rstan` or
`cmdstanr`. Backend portability should preserve the model family,
formula, priors, estimand and sampling contract. It should **not** imply
identical random-number streams or identical posterior draws.

``` r

backend_capabilities()
#> <gp3bayes_backend_capabilities_v2>
#>   backend brms_available backend_package_available backend_package_version
#>     rstan           TRUE                      TRUE                  2.32.7
#>  cmdstanr           TRUE                      TRUE                   0.9.0
#>  external_runtime_available external_runtime_version
#>                        TRUE                     <NA>
#>                       FALSE                     <NA>
#>  ready_for_package_interface algorithm
#>                         TRUE  sampling
#>                        FALSE  sampling
#>                                          model_family_scope
#>  Bernoulli-logit and positive uncensored lognormal duration
#>  Bernoulli-logit and positive uncensored lognormal duration
#>  unrestricted_modeling
#>                  FALSE
#>                  FALSE
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

An optional compiler smoke test can be requested explicitly and is not
run in this vignette:

``` r

validate_backend_environment("rstan", compile_test = TRUE)
validate_backend_environment("cmdstanr", compile_test = TRUE)
```

## Posterior-summary parity

Parity is evaluated relative to Monte Carlo uncertainty rather than
exact draw identity. The data-frame interface below makes the rule
transparent and is also useful for archived summary comparisons.

``` r

rstan_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.60, 0.40),
  sd = c(0.20, 0.15),
  mcse_mean = c(0.01, 0.01)
)

cmdstanr_summary <- data.frame(
  variable = c("b_Intercept", "b_conditiontreatment"),
  mean = c(-0.59, 0.41),
  sd = c(0.21, 0.15),
  mcse_mean = c(0.01, 0.01)
)

parity <- audit_backend_parity(
  rstan_summary,
  cmdstanr_summary
)
parity
#> <gp3bayes_backend_parity_audit>
#>   Status: pass
#>   Parameters compared: 2
#>   Review parameters: 0
#>   Identical draws expected: FALSE
plot(parity)
```

![](backend-reliability_files/figure-html/unnamed-chunk-3-1.png)

With real fits, the same function obtains posterior summaries from each
fit:

``` r

parity <- audit_backend_parity(
  fit_rstan,
  fit_cmdstanr,
  variables = c("b_Intercept", "b_conditiontreatment")
)
```

## Object-schema compatibility

A stable release also needs to know when serialized object structure
changes. Schema capture records structure rather than values.

``` r

contract <- create_model_contract(
  "binary", "selected", "participant_id",
  condition_col = "condition"
)

schema <- capture_gp3bayes_schema(contract)
schema
#> <gp3bayes_object_schema>
#>   Object class: gp3bayes_model_contract
#>   Recorded nodes: 41
#>   Maximum depth: 3
#>   Values recorded: FALSE

validation <- validate_gp3bayes_schema(contract, schema)
validation
#> <gp3bayes_schema_validation>
#>   Status: pass
#>   Schema compatibility only: TRUE
```

Freezing does not write anything unless a path is explicitly provided:

``` r

frozen_schema <- freeze_gp3bayes_schema(schema)

schema_file <- tempfile(fileext = ".rds")
freeze_gp3bayes_schema(frozen_schema, schema_file)
read_gp3bayes_schema(schema_file)
#> <gp3bayes_object_schema>
#>   Object class: gp3bayes_model_contract
#>   Recorded nodes: 41
#>   Maximum depth: 3
#>   Values recorded: FALSE
unlink(schema_file)
```

A schema match is a compatibility check only. It says nothing about
numerical identity, statistical adequacy, or scientific validity.

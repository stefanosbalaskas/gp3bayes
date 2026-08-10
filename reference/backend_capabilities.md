# Report Bayesian Backend Capabilities

Provides a stable 0.2.0-facing capability table for the two approved
Stan backends. It augments the existing package capability report with
package versions, CmdStan installation information, and explicit
readiness fields. No model is compiled or fitted.

## Usage

``` r
backend_capabilities()
```

## Value

A `gp3bayes_backend_capabilities_v2` data frame.

## Examples

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
```

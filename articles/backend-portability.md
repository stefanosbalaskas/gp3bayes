# Backend Portability and Installation

The advanced extension supports the two backends officially exposed by
brms: `rstan` and `cmdstanr`. The model family, formula, priors, and
algorithm remain restricted by gp3bayes. Only the implementation backend
is selectable.

## Audit installed components

``` r

bayesian_backend_capabilities()
#> 
#> Optional Bayesian capabilities
#> 
#>         component installed    version usable
#>              brms      TRUE     2.23.0   TRUE
#>             rstan      TRUE     2.32.7   TRUE
#>          cmdstanr      TRUE      0.9.0  FALSE
#>               loo      TRUE     2.10.1   TRUE
#>        priorsense      TRUE      1.2.0   TRUE
#>  detectseparation      TRUE      0.4.0   TRUE
#>               SBC      TRUE 0.5.0.9000   TRUE
#>                                                     detail
#>                                          package available
#>                                          package available
#>  CmdStan path has not been set yet. See ?set_cmdstan_path.
#>                                          package available
#>                                          package available
#>                                          package available
#>                                          package available
```

## CmdStanR setup

Install CmdStanR from the Stan R-universe repository:

``` r

install.packages(
  "cmdstanr",
  repos = c(
    "https://stan-dev.r-universe.dev",
    getOption("repos")
  )
)
```

Then check the C++ toolchain and install CmdStan:

``` r

cmdstanr::check_cmdstan_toolchain()
cmdstanr::install_cmdstan(cores = 4)
check_cmdstan_backend(strict = TRUE)
```

The gp3bayes installer never installs or repairs CmdStan automatically
unless that explicit option is enabled.

## Full MCMC only

``` r

fit <- fit_duration_model_backend(
  specification = duration_spec,
  backend = "cmdstanr",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 2026
)
```

Variational inference, Pathfinder, Laplace approximation, arbitrary Stan
code, and arbitrary backend arguments remain outside this wrapper.

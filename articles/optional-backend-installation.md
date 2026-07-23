# Optional Bayesian Backend Installation

## Core installation

The package is independently useful without a Bayesian backend. Model
contracts, readiness audits, deterministic simulation, preparation,
specification, and prior predictive checks do not require `brms`,
`rstan`, `posterior`, `bayesplot`, or a compiler.

``` r

install.packages(
  "gp3bayes",
  repos = NULL,
  type = "source"
)
```

## Optional fitting and validation dependencies

Full MCMC fitting and posterior validation require:

``` r

install.packages(
  c(
    "brms",
    "rstan",
    "posterior",
    "bayesplot"
  )
)
```

The supported fitting route is fixed to the `brms` interface, `rstan`
backend, and full sampling algorithm. `cmdstanr`, variational inference,
Pathfinder, Laplace approximation, and user-supplied Stan programs are
not part of the approved interface.

## Windows toolchain check

On Windows, source compilation requires the Rtools version compatible
with the installed R version. After installing Rtools, start a clean R
session and run:

``` r

pkgbuild::has_build_tools(
  debug = TRUE
)
```

The result should be `TRUE`. If Stan compilation has already occurred in
the current session and the probe unexpectedly includes Stan-specific
include paths, restart R and repeat the check in a clean session.

## Backend preflight

``` r

stopifnot(
  requireNamespace(
    "brms",
    quietly = TRUE
  ),
  requireNamespace(
    "rstan",
    quietly = TRUE
  ),
  requireNamespace(
    "posterior",
    quietly = TRUE
  )
)

pkgbuild::has_build_tools(
  debug = TRUE
)
```

## Minimal compilation smoke test

Compilation should be tested with a deliberately small synthetic model
before a large analysis. Short chains may produce low
effective-sample-size warnings; those warnings must not be interpreted
as adequate posterior inference.

``` r

simulation <- simulate_hierarchical_binary_data(
  n_participants = 8,
  trials_per_participant = 6,
  n_items = 4,
  random_slope_sd = 0,
  seed = 7001
)

contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c(
    "control",
    "treatment"
  )
)

specification <- specify_binary_model(
  prepared,
  baseline = 0.35
)

smoke_fit <- fit_binary_model(
  specification,
  chains = 2,
  iter = 300,
  warmup = 150,
  cores = 2,
  seed = 7002,
  refresh = 0
)
```

A successful smoke fit confirms compilation and sampling execution only.
Production analyses require adequate iterations, sampling diagnostics,
posterior predictive checks, sensitivity assessment, and transparent
reporting.

## Clean-process package checks

After a Stan fit on Windows, run package checks and pkgdown builds in
separate clean R processes. This avoids accidental inheritance of
model-compilation flags from the interactive session.

``` text
Rscript --vanilla -e "devtools::check()"
Rscript --vanilla -e "pkgdown::check_pkgdown(); pkgdown::build_site(preview = FALSE)"
```

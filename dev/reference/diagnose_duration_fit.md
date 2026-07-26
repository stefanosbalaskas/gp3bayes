# Diagnose a Fitted Duration Model

Applies the package sampling-diagnostic contract to an approved
hierarchical lognormal duration fit.

## Usage

``` r
diagnose_duration_fit(
  fit,
  rhat_pass = 1.01,
  rhat_fail = 1.05,
  ess_per_chain_pass = 100,
  ess_per_chain_fail = 50,
  maximum_treedepth_fraction = 0.01,
  ebfmi_pass = 0.3,
  ebfmi_fail = 0.2
)
```

## Arguments

- fit:

  A `gp3bayes_duration_fit`.

- rhat_pass:

  R-hat value at or below which the component passes.

- rhat_fail:

  R-hat value above which the component fails.

- ess_per_chain_pass:

  Bulk or tail ESS per chain at or above which the component passes.

- ess_per_chain_fail:

  Bulk or tail ESS per chain below which the component fails.

- maximum_treedepth_fraction:

  Maximum fraction of post-warmup draws that may reach the configured
  maximum treedepth before the component fails.

- ebfmi_pass:

  E-BFMI value at or above which the energy component passes.

- ebfmi_fail:

  E-BFMI value below which the energy component fails.

## Value

A `gp3bayes_duration_diagnostics` object.

## Details

The returned status reports prespecified numerical sampling thresholds.
It does not automatically establish convergence or posterior adequacy.

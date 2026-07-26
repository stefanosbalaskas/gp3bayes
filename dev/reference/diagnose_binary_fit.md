# Diagnose a Fitted Binary Model

Computes rank-normalized R-hat, bulk and tail effective sample sizes,
divergent-transition counts, maximum-treedepth saturation, and
chain-level energy diagnostics for an approved binary fit.

## Usage

``` r
diagnose_binary_fit(
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

  A fitted `gp3bayes_fit`.

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

A `gp3bayes_binary_diagnostics` object containing parameter, component,
and chain-level diagnostic tables.

## Details

The overall status is `"fail"` when any component fails, `"review"` when
any component requires review or cannot be assessed, and `"pass"` only
when every component passes. A pass does not automatically establish
convergence or posterior adequacy.

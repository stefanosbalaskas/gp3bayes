# Diagnose an advanced pupil fit

Diagnose an advanced pupil fit

## Usage

``` r
diagnose_advanced_pupil_fit(fit, rhat_threshold = 1.01, ess_threshold = 400)
```

## Arguments

- fit:

  An advanced pupil fit.

- rhat_threshold:

  Maximum preferred R-hat.

- ess_threshold:

  Minimum preferred bulk/tail ESS.

## Value

A `gp3bayes_pupil_advanced_diagnostics` object.

# Pupil posterior predictive checks and temporal diagnostics

## Posterior predictive evidence

Posterior predictive checks compare observed features with replicated
data. They are evidence objects, not automatic model-validity
certificates.

``` r

ppc <- check_pupil_posterior_predictive(
  pupil_fit,
  ndraws = 200,
  window = c(0.3, 1.0)
)
pupil_ppc_table(ppc)
plot_pupil_ppc(ppc)
```

The implementation summarizes observed and replicated trajectories,
declared window summaries, AUC, peak response and latency, residual
structure, and measurement-context overlays when corresponding
indicators are available.

## Temporal residual review

``` r

diag <- diagnose_pupil_fit(pupil_fit)
as.data.frame(diag)
acf_table <- pupil_residual_acf(pupil_fit, max_lag = 12)
head(acf_table)
plot_pupil_residual_acf(acf_table)
```

Sampling diagnostics reuse the package’s posterior/MCMC infrastructure
and report quantities such as R-hat, effective sample size, divergences,
treedepth, and available energy diagnostics. Temporal diagnostics
additionally show residual autocorrelation and support over
event-relative time.

No single threshold is labelled proof of model adequacy. Measurement
limitations, specification uncertainty, and the prediction target remain
separate questions.

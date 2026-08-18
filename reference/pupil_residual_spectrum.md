# Compute a descriptive residual spectrum

Provides a frequency-domain diagnostic for residual periodicity after
posterior mean subtraction. It does not infer physiological
oscillations.

## Usage

``` r
pupil_residual_spectrum(fit, ndraws = 300L)
```

## Arguments

- fit:

  An advanced fit.

- ndraws:

  Posterior expected-mean draws.

## Value

A `gp3bayes_pupil_residual_spectrum` object.

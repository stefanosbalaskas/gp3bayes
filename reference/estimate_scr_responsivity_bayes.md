# Bayesian retention-first SCR responsivity summary

Estimates participant-level response probabilities with a conjugate
Beta-Binomial model while preserving the conventional
amplitude-threshold non-responder flag. Low-reactive participants are
retained so hard exclusion can be evaluated as a sensitivity
specification rather than preprocessing.

## Usage

``` r
estimate_scr_responsivity_bayes(
  participant,
  amplitude,
  threshold = 0.02,
  prior_alpha = 1,
  prior_beta = 1,
  probability_cutoff = 0.5
)
```

## Arguments

- participant:

  Participant identifiers.

- amplitude:

  Trial-level SCR amplitudes.

- threshold:

  Conventional response threshold in microsiemens.

- prior_alpha, prior_beta:

  Positive Beta prior parameters.

- probability_cutoff:

  Optional posterior-probability cutoff used only for descriptive
  classification.

## Value

A data frame with posterior responsivity parameters and flags.

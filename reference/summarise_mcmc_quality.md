# Summarise MCMC Quality Evidence

Summarise MCMC Quality Evidence

## Usage

``` r
summarise_mcmc_quality(fit, ...)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- ...:

  Threshold arguments passed to
  [`identify_mcmc_issues()`](https://stefanosbalaskas.github.io/gp3bayes/reference/identify_mcmc_issues.md).

## Value

A `gp3bayes_mcmc_quality` object containing parameter and sampler
diagnostic evidence. It is a review object, not an adequacy certificate.

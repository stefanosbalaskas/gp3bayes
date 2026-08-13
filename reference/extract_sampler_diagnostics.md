# Extract NUTS Sampler Diagnostics

Extract NUTS Sampler Diagnostics

## Usage

``` r
extract_sampler_diagnostics(fit)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

## Value

A data frame returned from
[`brms::nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html).

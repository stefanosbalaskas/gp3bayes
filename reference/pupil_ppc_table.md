# Extract pupil PPC evidence tables

Extract pupil PPC evidence tables

## Usage

``` r
pupil_ppc_table(
  x,
  component = c("trajectory", "distribution", "features", "residuals",
    "residual_trajectory", "autocorrelation", "heterogeneity", "measurement_context")
)
```

## Arguments

- x:

  A pupil PPC object.

- component:

  One of `"trajectory"`, `"distribution"`, `"features"`, `"residuals"`,
  `"residual_trajectory"`, `"autocorrelation"`, `"heterogeneity"`, or
  `"measurement_context"`.

## Value

A data frame.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

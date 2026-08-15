# Plot pupil posterior-predictive evidence

Uses one consolidated plotting interface for trajectory, residual,
feature, autocorrelation, heterogeneity, or measurement-context PPC
evidence.

## Usage

``` r
plot_pupil_ppc(
  x,
  component = c("trajectory", "residuals", "features", "autocorrelation",
    "heterogeneity", "measurement_context")
)
```

## Arguments

- x:

  Pupil PPC object.

- component:

  Evidence component to plot.

## Value

A `ggplot` object.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

# Plot Sampling Diagnostics

Produces trace, energy, treedepth, or divergence plots for an approved
fitted `gp3bayes` model.

## Usage

``` r
plot_sampling_diagnostics(
  fit,
  type = c("trace", "energy", "treedepth", "divergence"),
  variables = NULL
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- type:

  One of `"trace"`, `"energy"`, `"treedepth"`, or `"divergence"`.

- variables:

  Optional posterior parameter names used for trace plots.

## Value

A plot object created by `bayesplot`.

## Details

Diagnostic plots support interpretation of sampling behaviour. They do
not establish convergence or substantive model adequacy by themselves.

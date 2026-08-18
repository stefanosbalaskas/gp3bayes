# Create a pre-fit advanced pupillometry sensitivity suite

The suite materializes scientifically interpretable alternative model
specifications without fitting, ranking, or choosing among them.

## Usage

``` r
create_pupil_advanced_sensitivity_suite(
  specification,
  include = c("likelihood", "residual_scale", "autocorrelation", "temporal", "gp_kernel")
)
```

## Arguments

- specification:

  Baseline advanced specification.

- include:

  Character subset of `"likelihood"`, `"residual_scale"`,
  `"autocorrelation"`, `"temporal"`, and `"gp_kernel"`.

## Value

A `gp3bayes_pupil_advanced_sensitivity_suite` object.

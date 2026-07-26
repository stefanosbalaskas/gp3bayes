# Specify a Binary Model with a Separate Interaction Prior

Retains the approved binary model contract while recording a common
main-effect prior scale and a narrower coefficient-specific interaction
prior scale.

## Usage

``` r
specify_binary_model_with_interaction_prior(
  prepared,
  baseline,
  intercept_scale = 1.5,
  main_effect_scale = 0.75,
  interaction_scale = 0.5,
  group_sd_scale = 1,
  correlation_eta = 2,
  student_df = 3
)
```

## Arguments

- prepared:

  A `gp3bayes_binary_prepared` object containing one approved two-way
  interaction.

- baseline:

  Plausible baseline event probability.

- intercept_scale:

  Intercept-prior scale.

- main_effect_scale:

  Common population main-effect prior scale.

- interaction_scale:

  Interaction prior scale.

- group_sd_scale:

  Group-level standard-deviation prior scale.

- correlation_eta:

  LKJ shape.

- student_df:

  Student-t degrees of freedom.

## Value

A binary model specification with advanced prior metadata.

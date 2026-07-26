# Create a Predictor-Scaling Sensitivity Specification

Changes one already-scaled predictor by a declared scale factor and
requires an explicit coefficient-prior scale for the new
parameterisation. This avoids pretending that a common coefficient prior
is automatically invariant to predictor scaling.

## Usage

``` r
create_predictor_scaling_sensitivity_specification(
  specification,
  predictor,
  scale_factor,
  coefficient_scale,
  interaction_scale = NULL
)
```

## Arguments

- specification:

  An approved model specification.

- predictor:

  A declared predictor that was scaled during preparation.

- scale_factor:

  New scale divided by the original recorded scale. Values above one
  make the transformed predictor numerically smaller.

- coefficient_scale:

  Explicit population-coefficient prior scale under the alternative
  parameterisation.

- interaction_scale:

  Optional explicit interaction prior scale when the advanced
  separate-interaction prior is used.

## Value

An approved alternative specification.

# Create an explicit bounded ARMA configuration

Create an explicit bounded ARMA configuration

## Usage

``` r
create_pupil_arma_spec(p = 1L, q = 0L, covariance = FALSE)
```

## Arguments

- p:

  Autoregressive order, constrained to 0–3.

- q:

  Moving-average order, constrained to 0–2.

- covariance:

  Logical; request covariance-form ARMA. In gp3bayes 0.5 this is
  permitted only for order (1,0), (0,1), or (1,1).

## Value

A `gp3bayes_pupil_arma_spec` object.

# Create a Diagnostic Dashboard Object

Expensive analyses are never launched implicitly. Supply
already-computed evidence objects.

## Usage

``` r
create_diagnostic_dashboard(
  fit = NULL,
  analysis_bundle = NULL,
  model_card = NULL,
  loo = NULL,
  prior_posterior = NULL,
  sensitivity = NULL,
  recovery = NULL,
  sbc = NULL,
  label = NULL
)
```

## Arguments

- fit:

  Optional fitted model.

- analysis_bundle:

  Optional analysis bundle.

- model_card:

  Optional model card.

- loo:

  Optional PSIS-LOO or LOO influence atlas.

- prior_posterior:

  Optional prior-posterior bridge.

- sensitivity:

  Optional sensitivity result.

- recovery:

  Optional recovery result.

- sbc:

  Optional SBC result.

- label:

  Optional label.

## Value

A `gp3bayes_diagnostic_dashboard`.

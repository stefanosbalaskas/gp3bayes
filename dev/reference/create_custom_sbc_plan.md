# Create an Expert Custom SBC Plan

Wraps an independently coded generator function and a user-supplied SBC
backend. The generator must return `variables` and `generated` elements
as required by SBC.

## Usage

``` r
create_custom_sbc_plan(
  generator_function,
  backend,
  n_sims = 20L,
  generator_args = list(),
  seed = 1L
)
```

## Arguments

- generator_function:

  Function producing one SBC dataset.

- backend:

  A valid SBC backend object.

- n_sims:

  Number of datasets.

- generator_args:

  Named list passed to the generator constructor.

- seed:

  Seed used for dataset generation.

## Value

A `gp3bayes_sbc_plan`.

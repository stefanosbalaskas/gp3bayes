# Create a Power-Scaling Sequence

Create a Power-Scaling Sequence

## Usage

``` r
powerscale_sequence_for_fit(
  fit,
  variable = NULL,
  prior_selection = NULL,
  likelihood_selection = NULL,
  component = "both"
)
```

## Arguments

- fit:

  A gp3bayes fit or `brmsfit`.

- variable:

  Optional posterior variables to inspect.

- prior_selection:

  Optional tagged priors to perturb.

- likelihood_selection:

  Optional likelihood subset.

- component:

  Either prior, likelihood, or both, as supported by priorsense.

## Value

A priorsense powerscaled sequence.

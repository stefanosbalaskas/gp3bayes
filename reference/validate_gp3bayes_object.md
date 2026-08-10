# Validate a gp3bayes Object

Performs lightweight structural validation for gp3bayes contracts,
prepared data, specifications, fits, summaries, diagnostics, manifests,
design audits, sensitivity suites, evidence collections, and backend
reliability objects. The function checks object structure only; it does
not establish statistical adequacy or substantive validity.

## Usage

``` r
validate_gp3bayes_object(x, recursive = TRUE, strict = FALSE)
```

## Arguments

- x:

  A gp3bayes object.

- recursive:

  Whether nested contract/specification/prepared objects should also be
  checked when present.

- strict:

  Whether a failed structural check should raise an error.

## Value

A `gp3bayes_object_validation` object.

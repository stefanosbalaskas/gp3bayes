# Materialize one declared pupil sensitivity scenario

Creates the alternate prepared/specification state for a declared
scenario. This does not fit the model. Analysis-window scenarios are
returned as estimand instructions. PFE scenarios select only a
user-supplied upstream prepared alternative; no PFE correction is
performed by gp3bayes.

## Usage

``` r
materialize_pupil_sensitivity_scenario(suite, scenario_id)
```

## Arguments

- suite:

  Pupil sensitivity suite.

- scenario_id:

  Scenario identifier from
  [`pupil_sensitivity_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/pupil_sensitivity_table.md).

## Value

A list containing the materialized specification and/or declared
estimand window.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```

# Run Detailed Binary Posterior Predictive Checks

Adds calibration bins, participant/item rate checks, focal-condition
rates, sparse participant-condition cells, and all-zero/all-one
participant patterns to the existing binary PPC workflow.

## Usage

``` r
check_binary_ppc_details(
  fit,
  draws = 300L,
  seed = 1L,
  calibration_bins = 10L,
  sparse_cell_min = 3L
)
```

## Arguments

- fit:

  Approved binary fit.

- draws:

  Number of posterior predictive draws.

- seed:

  Random seed.

- calibration_bins:

  Number of probability calibration bins.

- sparse_cell_min:

  Cell size below which participant-condition cells are reported as
  sparse.

## Value

A `gp3bayes_binary_ppc_detail`.

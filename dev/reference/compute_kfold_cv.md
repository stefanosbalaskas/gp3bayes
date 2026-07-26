# Compute Governed Exact K-Fold Cross-Validation

Uses
[`brms::kfold()`](https://mc-stan.org/loo/reference/kfold-generic.html)
as an explicit fallback or complement to PSIS-LOO. Grouped folds may use
only the declared participant or item grouping column. No model is
selected automatically.

## Usage

``` r
compute_kfold_cv(
  fit,
  K = 10L,
  folds = c("random", "stratified", "grouped"),
  group = NULL,
  joint = c("obs", "fold", "group"),
  save_fits = FALSE,
  seed = 1L
)
```

## Arguments

- fit:

  Approved gp3bayes fit.

- K:

  Number of folds for random or stratified splitting.

- folds:

  One of `"random"`, `"stratified"`, or `"grouped"`.

- group:

  Optional declared grouping column used by stratified/grouped
  splitting.

- joint:

  One of `"obs"`, `"fold"`, or `"group"`.

- save_fits:

  Whether cross-validation refits are retained.

- seed:

  Random seed.

## Value

A `gp3bayes_kfold_cv`.

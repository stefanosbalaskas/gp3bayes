# Binary Prediction Scores

Binary Prediction Scores

## Usage

``` r
binary_prediction_scores(x, observed = NULL, threshold = 0.5, epsilon = 1e-12)
```

## Arguments

- x:

  A binary expected-response `gp3bayes_prediction` or numeric event
  probabilities.

- observed:

  Optional binary outcomes when `x` is numeric.

- threshold:

  Classification threshold used only for threshold summaries.

- epsilon:

  Probability truncation used for finite log loss.

## Value

A one-row data frame of descriptive predictive scores.

## Examples

``` r
binary_prediction_scores(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#>   n brier  log_loss auc threshold accuracy sensitivity specificity
#> 1 4 0.045 0.2270806   1       0.5        1           1           1
#>   balanced_accuracy automatic_decision
#> 1                 1              FALSE
```

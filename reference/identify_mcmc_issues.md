# Identify MCMC Diagnostic Flags

Identify MCMC Diagnostic Flags

## Usage

``` r
identify_mcmc_issues(
  x,
  rhat_threshold = 1.01,
  min_bulk_ess = 400,
  min_tail_ess = 400,
  max_mcse_fraction = 0.1
)
```

## Arguments

- x:

  A fitted gp3bayes object or an MCMC diagnostic table.

- rhat_threshold:

  R-hat value above which a parameter is flagged.

- min_bulk_ess:

  Minimum bulk effective sample size.

- min_tail_ess:

  Minimum tail effective sample size.

- max_mcse_fraction:

  Maximum MCSE-to-posterior-SD fraction.

## Value

A parameter-level flag table. Flags request review; they do not
establish or negate model adequacy.

## Examples

``` r
d <- data.frame(
  variable = c("a", "b"),
  sd = c(1, 1),
  rhat = c(1.00, 1.03),
  ess_bulk = c(1000, 150),
  ess_tail = c(900, 120),
  mcse_mean = c(0.02, 0.15)
)
identify_mcmc_issues(d)
#>   variable rhat ess_bulk ess_tail mcse_fraction rhat_flag bulk_ess_flag
#> 1        a 1.00     1000      900          0.02     FALSE         FALSE
#> 2        b 1.03      150      120          0.15      TRUE          TRUE
#>   tail_ess_flag mcse_flag flagged
#> 1         FALSE     FALSE   FALSE
#> 2          TRUE      TRUE    TRUE
```

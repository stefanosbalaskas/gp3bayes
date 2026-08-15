# Inspect verified Gazepoint pupil and gaze fields

Compares column names with the documented Open Gaze API field
identifiers. The inspector reports candidates and ambiguity but never
chooses a pupil channel automatically. Export variants that use other
names remain unrecognized rather than being guessed.

## Usage

``` r
inspect_gazepoint_pupil_schema(data)
```

## Arguments

- data:

  A data frame containing a Gazepoint export or API record table.

## Value

A `gp3bayes_gazepoint_pupil_schema` with detected fields, pupil-channel
candidates, and an audit table.

## Details

`LPD` and `RPD` are documented pixel diameters. `LPUPILD` and `RPUPILD`
are documented in metres. These scales are intentionally kept distinct.

## Governance boundary

Recognition is schema evidence only. The function does not establish
device validity, select an eye, convert units, correct PFE, or infer
preprocessing history.

## Examples

``` r
x <- data.frame(TIME = 0:2 / 60, LPD = c(32, 33, 31), LPV = 1)
inspect_gazepoint_pupil_schema(x)
#> <gp3bayes_gazepoint_pupil_schema>
#>   Status: single_pupil_candidate
#>   Documented fields detected: 3
#>   Pupil candidates: 1
#>   Candidates: LPD
#>   Automatic channel selection: FALSE
```

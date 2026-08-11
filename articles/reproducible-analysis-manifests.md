# Analysis Manifests and Reproducible Bayesian Workflows

## A manifest is an analysis contract about the analysis contract

A fitted model is not enough to reconstruct an analysis decision
process. `gp3bayes` 0.2.0 therefore provides an analysis manifest that
records the approved model contract, preparation/transformation record,
specification, prespecified estimands, sensitivity plan, seed, backend
metadata, software versions, and a fingerprint of the analysis data.

The manifest stores a fingerprint rather than duplicating the analysis
data. It is provenance metadata, not a hidden data archive.

## Create a manifest before fitting

``` r

simulation <- simulate_hierarchical_binary_data(
  n_participants = 10,
  trials_per_participant = 8,
  n_items = 5,
  random_slope_sd = 0,
  seed = 42
)

contract <- create_model_contract(
  "binary", "selected", "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment")
)

specification <- specify_binary_model(prepared, baseline = 0.35)

manifest <- create_analysis_manifest(
  specification = specification,
  estimands = "standardized_probability_contrast",
  seed = 2026,
  label = "Synthetic binary release case"
)

manifest
#> <gp3bayes_analysis_manifest>
#>   Version: 0.2
#>   Label: Synthetic binary release case
#>   Family: binary
#>   Data: 80 x 8
#>   Data hash: 09ab941d91559d015dc9fe304ebfb2e7
#>   Frozen: FALSE
analysis_manifest_table(manifest)
#>              component                            value
#> 1               family                           binary
#> 2         model_family              hierarchical_binary
#> 3            data_hash 09ab941d91559d015dc9fe304ebfb2e7
#> 4        contract_hash a004cd56a9d35cff138e36f46a836579
#> 5   specification_hash 309b774249f3f72e44c7e76d8a98f318
#> 6  transformation_hash bb87585a96dfafaf1dc60c74014e0038
#> 7                 seed                             2026
#> 8              backend                             <NA>
#> 9               frozen                            FALSE
#> 10       manifest_hash                             <NA>
validate_analysis_manifest(manifest)
#> <gp3bayes_manifest_validation>
#>   Status: pass
#>             check status                           detail
#>    manifest_class   pass       gp3bayes_analysis_manifest
#>   required_fields   pass                         complete
#>  data_fingerprint   pass 09ab941d91559d015dc9fe304ebfb2e7
#>   approved_family   pass                           binary
```

## Freeze only when the analysis-defining fields are ready

Freezing computes a manifest hash. With `file = NULL`, no file is
written.

``` r

frozen <- freeze_analysis_manifest(manifest)
frozen
#> <gp3bayes_analysis_manifest>
#>   Version: 0.2
#>   Label: Synthetic binary release case
#>   Family: binary
#>   Data: 80 x 8
#>   Data hash: 09ab941d91559d015dc9fe304ebfb2e7
#>   Frozen: TRUE
#>   Manifest hash: 1b0bdea4ffa06171ed35d42cf4076253
```

Writing is always explicit. Temporary files are used here so the
vignette does not write into the package or working directory.

``` r

manifest_file <- tempfile(fileext = ".rds")
report_file <- tempfile(fileext = ".md")

freeze_analysis_manifest(manifest, file = manifest_file)
#> <gp3bayes_analysis_manifest>
#>   Version: 0.2
#>   Label: Synthetic binary release case
#>   Family: binary
#>   Data: 80 x 8
#>   Data hash: 09ab941d91559d015dc9fe304ebfb2e7
#>   Frozen: TRUE
#>   Manifest hash: 1b0bdea4ffa06171ed35d42cf4076253
restored <- read_analysis_manifest(manifest_file)
write_reproducibility_report(restored, report_file)

file.exists(manifest_file)
#> [1] TRUE
file.exists(report_file)
#> [1] TRUE

unlink(c(manifest_file, report_file))
```

## Compare analysis provenance

A difference is reported, not judged automatically.

``` r

alternative <- create_analysis_manifest(
  specification = specification,
  estimands = "standardized_probability_contrast",
  seed = 2027,
  label = "Alternative seed"
)

comparison <- compare_analysis_manifests(manifest, alternative)
comparison
#> <gp3bayes_manifest_comparison>
#>   Identical: FALSE
#>   Changed: seed
plot(comparison)
```

![](reproducible-analysis-manifests_files/figure-html/unnamed-chunk-4-1.png)

This comparison is particularly useful during revisions, refits, or a
package upgrade: it makes changes to the data fingerprint,
transformations, priors, estimands, seed, backend settings, or software
environment visible without pretending that every difference is
scientifically consequential.

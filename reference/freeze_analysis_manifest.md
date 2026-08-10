# Freeze an Analysis Manifest

Computes a deterministic hash over analysis-defining fields. When `file`
is supplied the frozen manifest is written explicitly to that path. No
file is written when `file = NULL`.

## Usage

``` r
freeze_analysis_manifest(manifest, file = NULL, overwrite = FALSE)
```

## Arguments

- manifest:

  A valid analysis manifest.

- file:

  Optional explicit `.rds` output path.

- overwrite:

  Whether an existing explicit output file may be replaced.

## Value

A frozen `gp3bayes_analysis_manifest`.

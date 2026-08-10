# Write a Reproducibility Report

Writes a conservative Markdown provenance report to an explicit path.

## Usage

``` r
write_reproducibility_report(manifest, file, overwrite = FALSE)
```

## Arguments

- manifest:

  An analysis manifest.

- file:

  Explicit Markdown output path.

- overwrite:

  Whether an existing file may be replaced.

## Value

The normalized output path, invisibly.

# Freeze a gp3bayes Object Schema

Marks a captured schema as frozen and optionally writes it to an
explicit RDS path. When `file = NULL`, no file is written.

## Usage

``` r
freeze_gp3bayes_schema(schema, file = NULL, overwrite = FALSE)
```

## Arguments

- schema:

  A captured schema or gp3bayes object.

- file:

  Optional explicit `.rds` path.

- overwrite:

  Whether an existing file may be replaced.

## Value

The frozen schema, invisibly when written.

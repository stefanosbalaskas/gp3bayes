# Save a Figure Set

File output is explicit: no current-directory default is provided.

## Usage

``` r
save_figure_set(
  x,
  directory,
  width = 7,
  height = 5,
  dpi = 300,
  device = "png",
  overwrite = FALSE
)
```

## Arguments

- x:

  A `gp3bayes_figure_set`.

- directory:

  Existing or creatable output directory.

- width, height:

  Figure dimensions in inches.

- dpi:

  Raster resolution.

- device:

  File extension/device, such as `"png"` or `"pdf"`.

- overwrite:

  Whether existing files may be replaced.

## Value

Invisibly, the written file paths.

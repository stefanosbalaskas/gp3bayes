# Create a Named Figure Set

Create a Named Figure Set

## Usage

``` r
create_figure_set(..., title = "gp3bayes figure set")
```

## Arguments

- ...:

  Named plot objects.

- title:

  Optional figure-set title.

## Value

A `gp3bayes_figure_set`.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  create_figure_set(example = p)
}
#> 
#> gp3bayes figure set
#>  Figures: 1
#>  example
```

# Check the CmdStanR Backend

Checks package availability, the C++ toolchain, and a configured CmdStan
installation. No installation or repair is performed automatically.

## Usage

``` r
check_cmdstan_backend(strict = FALSE)
```

## Arguments

- strict:

  Whether to stop instead of returning a failed audit.

## Value

A list with status, version, path, and diagnostic detail.

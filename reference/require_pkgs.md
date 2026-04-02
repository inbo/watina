# Check availability of required packages

Takes a vector of package names and passes each name to
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html); if
package(s) are missing, returns an error message providing the basic
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
command to install them.

## Usage

``` r
require_pkgs(pkgs)
```

## Arguments

- pkgs:

  A character vector of package names.

## Examples

``` r
if (FALSE) { # \dontrun{
require_pkgs(c("a", "base", "b", "c"))
} # }
```

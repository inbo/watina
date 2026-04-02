# Connect to the INBO Watina data warehouse

Returns a connection to the INBO **Watina** data warehouse. The function
can only be used from within the INBO network.

## Usage

``` r
connect_watina()
```

## Value

A `DBIConnection` object.

## Details

Don't forget to disconnect at the end of your R-script using
[`dbDisconnect`](https://inbo.github.io/watina/reference/dbDisconnect.md)!

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
# Do your stuff.
# Disconnect:
dbDisconnect(watina)
} # }
```

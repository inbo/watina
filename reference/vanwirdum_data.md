# Return the 'vanwirdum_data' data source as a tibble

Returns the included data source 'vanwirdum_data' as a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Usage

``` r
vanwirdum_data
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 42
rows and 2 columns.

## Value

The `vanwirdum_data` dataframe as a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)

## Details

The 'vanwirdum_data' dataset gives the curved contour which encloses the
plotting area of all possible, simple mixtures of the following
reference water samples:

\- LI-ANG (a relatively calcium-rich groundwater sample),

\- AT-WTV (a precipitation sample caught in a relatively unpolluted
inland area of The Netherlands)

\- TH-N70 (a representative analysis from the North Sea monitoring
program, 70 km from the coast).

The curved contour around these points gives the boundary within which
most natural waters can occur.

`vanwirdum_data` returns this dataset as a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with 42 rows and 2 variables. A tibble is a dataframe that makes working
in the tidyverse a little [easier](https://r4ds.had.co.nz/tibbles.html).

Variables:

- ec25:

  electrical conductivity at 25°C in mS/m

- ir:

  ionic ratio in %

## References

Van Wirdum, Geert (1991). Vegetation and hydrology of floating
rich-fens. Datawyse, Maastricht. 316 p. ISBN 90-5291-045-6. (Appendix D)
[dataset available
here](https://publicwiki.deltares.nl/display/VWD/Home)

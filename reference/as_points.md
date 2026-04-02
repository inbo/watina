# Convert a data frame with X and Y coordinates to a geospatial points object

`as_points` is a convenience function which accepts as input a data
frame with X/Y coordinates (in meters), assumed to come from the
coordinate reference system (CRS) 'Belge 1972 / Belgian Lambert 72'
(EPSG [31370](https://epsg.io/31370)). It converts the data frame into
an `sf` points object in the same CRS.

## Usage

``` r
as_points(df, xvar = "x", yvar = "y", remove = FALSE, warn_dupl = TRUE)
```

## Arguments

- df:

  A data frame with X and Y coordinates in meters, assumed to be in the
  Belgian Lambert 72 CRS (EPSG-code 31370).

- xvar:

  String. The X coordinate variable name. Defaults to `"x"`.

- yvar:

  String. The Y coordinate variable name. Defaults to `"y"`.

- remove:

  Logical. Should the X and Y coordinates be removed from the data frame
  after conversion to a spatial object?

- warn_dupl:

  Logical. Defaults to `TRUE`. Should the user be warned when duplicated
  coordinates are present in the result?

## Value

An `sf` object of geometry type `POINT` with EPSG-code 31370 as
coordinate reference system.

## Details

As locations in Watina are typically defined by their X/Y coordinates,
this function eases the conversion to spatial data. To later remove all
spatial information from the result, you can use
[`sf::st_drop_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html).

## Examples

``` r
library(tibble)
mydata <-
  tibble(
    a = runif(5),
    x = rnorm(5, 155763, 5),
    y = rnorm(5, 132693, 5)
  )
as_points(mydata)
#> Simple feature collection with 5 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 155758.3 ymin: 132685.4 xmax: 155766.7 ymax: 132702.8
#> Projected CRS: BD72 / Belgian Lambert 72
#> # A tibble: 5 × 4
#>         a       x       y            geometry
#> *   <dbl>   <dbl>   <dbl>         <POINT [m]>
#> 1 0.0808  155763. 132689. (155762.6 132688.7)
#> 2 0.834   155760. 132685. (155760.2 132685.4)
#> 3 0.601   155767. 132703. (155766.7 132702.8)
#> 4 0.157   155758. 132695. (155758.3 132695.3)
#> 5 0.00740 155761. 132689. (155760.7 132688.7)
```

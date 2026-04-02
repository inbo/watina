# Detect (spatial) groundwater well clusters

`cluster_locs()` accepts as input a data frame with X/Y coordinates, or
an `sf` object of geometry type `POINT`. The function adds an integer
variable that defines cluster membership. The intention is to detect
spatial groundwater well clusters; hence it uses a sensible method of
spatial clustering and default euclidean distance to cut the cluster
tree.

## Usage

``` r
cluster_locs(
  input,
  max_dist = 2,
  output_var = "cluster_id",
  xvar = "x",
  yvar = "y"
)
```

## Arguments

- input:

  A data frame with X/Y coordinates, or an `sf` object of geometry type
  `POINT`. A typical input data frame is the collected output of
  [`get_locs`](https://inbo.github.io/watina/reference/get_locs.md).

- max_dist:

  The maximum geospatial distance between two points to make them belong
  to the same cluster. The default value is sensible for many usecases,
  supposing *meter* is the unit of the coordinate reference system, as
  is the case for the 'Belge 1972 / Belgian Lambert 72' CRS (EPSG
  [31370](https://epsg.io/31370)).

- output_var:

  Name of the new variable to be added to `input`.

- xvar:

  String. The X coordinate variable name; only considered when `input`
  is a data frame. Defaults to `"x"`.

- yvar:

  String. The Y coordinate variable name; only considered when `input`
  is a data frame. Defaults to `"y"`.

## Value

The original object with an extra variable added (by default:
`cluster_id`) to define cluster membership.

## Details

The function performs agglomerative clustering with the *complete
linkage* method. This way, the application of a tree cutoff (`max_dist`)
means that each cluster is a collection of locations with a maximum
distance - between any two locations of the cluster - not larger than
the cutoff value. All locations that can be clustered under this
condition, will be. Locations that can not be clustered receive a unique
cluster value.

The function's code was partly inspired by unpublished code from Ivy
Jansen.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
set.seed(123456789)
mydata <-
  tibble(
    a = runif(10),
    x = rnorm(10, 155763, 2),
    y = rnorm(10, 132693, 2)
  )
cluster_locs(mydata) %>%
  arrange(cluster_id)
#> # A tibble: 10 × 4
#>        a       x       y cluster_id
#>    <dbl>   <dbl>   <dbl>      <int>
#>  1 0.693 155760. 132690.          1
#>  2 0.931 155761. 132689.          1
#>  3 0.673 155763. 132691.          2
#>  4 0.453 155763. 132692.          2
#>  5 0.654 155763. 132695.          3
#>  6 0.719 155760. 132695.          4
#>  7 0.899 155761. 132694.          4
#>  8 0.922 155765. 132689.          5
#>  9 0.235 155765. 132695.          6
#> 10 0.268 155761. 132693.          7
mydata %>%
  as_points(remove = TRUE) %>%
  cluster_locs() %>%
  arrange(cluster_id)
#> Simple feature collection with 10 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 155759.9 ymin: 132689.1 xmax: 155765.3 ymax: 132695.4
#> Projected CRS: BD72 / Belgian Lambert 72
#> # A tibble: 10 × 3
#>        a            geometry cluster_id
#>    <dbl>         <POINT [m]>      <int>
#>  1 0.693 (155759.9 132690.4)          1
#>  2 0.931 (155760.8 132689.4)          1
#>  3 0.673 (155763.3 132690.8)          2
#>  4 0.453 (155763.4 132692.3)          2
#>  5 0.654 (155762.7 132695.3)          3
#>  6 0.719   (155760 132695.4)          4
#>  7 0.899 (155760.9 132694.5)          4
#>  8 0.922 (155765.3 132689.1)          5
#>  9 0.235 (155765.1 132695.1)          6
#> 10 0.268   (155761.1 132693)          7

if (FALSE) { # \dontrun{
watina <- connect_watina()

clusters <-
  get_locs(watina,
    area_codes = "KBR",
    collect = TRUE
  ) %>%
  cluster_locs()

# inspect result:
clusters %>%
  select(loc_code, x, y, cluster_id) %>%
  arrange(cluster_id)

# frequency of cluster sizes:
clusters %>%
  count(cluster_id) %>%
  pull(n) %>%
  table()

# Disconnect:
dbDisconnect(watina)
} # }
```

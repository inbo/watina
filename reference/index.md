# Package index

## Connect

- [`connect_watina()`](https://inbo.github.io/watina/reference/connect_watina.md)
  : Connect to the INBO Watina data warehouse
- [`dbDisconnect`](https://inbo.github.io/watina/reference/dbDisconnect.md)
  : Disconnect a database connection

## Get locations

- [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md) :
  Get locations from the data warehouse

## Processing XG3 (HG3, LG3, VG3)

- [`get_xg3()`](https://inbo.github.io/watina/reference/get_xg3.md) :
  Get XG3 values from the data warehouse
- [`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
  : Evaluate the availability of XG3 values per location
- [`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
  : Identify and evaluate XG3 series per location
- [`selectlocs_xg3()`](https://inbo.github.io/watina/reference/selectlocs_xg3.md)
  : Select locations based on XG3 availability and XG3 series'
  properties

## Processing hydrochemical data

- [`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md) :
  Get hydrochemical data from the data warehouse
- [`eval_chem()`](https://inbo.github.io/watina/reference/eval_chem.md)
  : Evaluate hydrochemical data per location
- [`selectlocs_chem()`](https://inbo.github.io/watina/reference/selectlocs_chem.md)
  : Select locations based on hydrochemical data properties

## Plot data

- [`ggplot_vanwirdum_background()`](https://inbo.github.io/watina/reference/ggplot_vanwirdum_background.md)
  : Plots hydrochemistry: Van Wirdum diagram

## Utilities and helpers

- [`as_points()`](https://inbo.github.io/watina/reference/as_points.md)
  : Convert a data frame with X and Y coordinates to a geospatial points
  object
- [`cluster_locs()`](https://inbo.github.io/watina/reference/cluster_locs.md)
  : Detect (spatial) groundwater well clusters
- [`extract_xg3_series()`](https://inbo.github.io/watina/reference/extract_xg3_series.md)
  : Identify XG3 series per location
- [`calculate_ir()`](https://inbo.github.io/watina/reference/calculate_ir.md)
  : Compute the ionic ratio

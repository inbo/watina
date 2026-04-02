# Changelog

## watina 0.5.0 (2026-04-01)

#### New features

This release provides new functionality with regard to ionic ratios and
plotting them in a Van Wirdum diagram
([\#114](https://github.com/inbo/watina/issues/114)):

- New function
  [`calculate_ir()`](https://inbo.github.io/watina/reference/calculate_ir.md)
  to compute the ionic ratio.
- New function
  [`ggplot_vanwirdum_background()`](https://inbo.github.io/watina/reference/ggplot_vanwirdum_background.md)
  to get a Van Wirdum diagram as ggplot object (without custom data), to
  which data layers can be added by the user.
- New vignette on plotting chemistry data:
  [`vignette("v230_chem_plots")`](https://inbo.github.io/watina/articles/v230_chem_plots.md).

#### Improvements and fixes

- [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)
  now substitutes filterlength values of 0 m by 0.3 m
  ([\#108](https://github.com/inbo/watina/issues/108)). Before, this was
  only done for missing filterlengths.
- Harden
  [`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md)
  with regard to the iron / conductivity ratio calculation (used by the
  `en_fecond_threshold` argument)
  ([\#122](https://github.com/inbo/watina/issues/122)):
  - Limit the calculation to the requested locations.
  - Set as missing if conductivity is set as zero in the data warehouse,
    and present a warning in case this occurs.
- Apply tidyverse code style
  ([\#119](https://github.com/inbo/watina/issues/119)).
- Various maintenance.

## watina 0.4.2 (2023-09-15)

- Move package {KSgeneral} to `Suggests`
  ([\#103](https://github.com/inbo/watina/issues/103)). This avoids
  package breaking when {KSgeneral} is not available on CRAN. For the
  moment not taking further measures to protect against it, as the
  package is currently back on CRAN.

## watina 0.4.1 (2021-06-11)

- Fixed non-working
  [`get_xg3()`](https://inbo.github.io/watina/reference/get_xg3.md) and
  [`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md)
  for data frame input, by avoiding the currently defunct
  `dbplyr::db_drop_table()`
  ([\#89](https://github.com/inbo/watina/issues/89),
  [08bc66d](https://github.com/inbo/watina/commit/08bc66d)).
- Various maintenance ([\#81](https://github.com/inbo/watina/issues/81),
  [\#86](https://github.com/inbo/watina/issues/86),
  [\#87](https://github.com/inbo/watina/issues/87),
  [\#88](https://github.com/inbo/watina/issues/88),
  [c382499](https://github.com/inbo/watina/commit/c382499)).

## watina 0.4.0 (2021-01-18)

- This release has been made compatible with `dbplyr` 2.0.0 (on CRAN);
  the `dbplyr` fork is not needed anymore
  ([e66e58f](https://github.com/inbo/watina/commit/e66e58f),
  [\#74](https://github.com/inbo/watina/issues/74)).
  - Follow the installation procedure on the homepage (readme) to
    upgrade.
  - Lazy results of
    [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md),
    [`get_xg3()`](https://inbo.github.io/watina/reference/get_xg3.md)
    and
    [`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md)
    are not sorted anymore. Sorting is done in tibbles only, i.e. if
    `collect = TRUE`. For more information, consult the *Note* added in
    the documentation of these functions.
- Fix broken
  [`selectlocs_xg3()`](https://inbo.github.io/watina/reference/selectlocs_xg3.md)
  and
  [`selectlocs_chem()`](https://inbo.github.io/watina/reference/selectlocs_chem.md)
  ([\#73](https://github.com/inbo/watina/issues/73)).
- Fix
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)
  error when no locations remain after spatial masking
  ([c96421e](https://github.com/inbo/watina/commit/c96421e)).
- Fix
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)
  error when `obswell_aggr = "mean"`
  ([\#80](https://github.com/inbo/watina/issues/80)).
- Adopt further [`inbodb`](https://inbo.github.io/inbodb) functionality
  ([\#75](https://github.com/inbo/watina/issues/75)):
  - re-export its
    [`dbDisconnect()`](https://inbo.github.io/watina/reference/dbDisconnect.md);
  - transfer handling of character encoding to `inbodb`.
- Improve documentation and `pkgdown` website
  ([\#76](https://github.com/inbo/watina/issues/76),
  [\#77](https://github.com/inbo/watina/issues/77),
  [3986b4e](https://github.com/inbo/watina/commit/3986b4e)).

## watina 0.3.0 (2020-05-20)

#### New features

- New function
  [`cluster_locs()`](https://inbo.github.io/watina/reference/cluster_locs.md)
  to spatially cluster locations (well clusters)
  ([\#39](https://github.com/inbo/watina/issues/39))
- More fun with
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md):
  - optionally allow groundwater piezometers with missing `filterdepth`
    (argument `filterdepth_na`)
  - optionally replace missing `filterdepth` values in a conservative
    way, based on `tubelength` (argument `filterdepth_guess`,
    [\#44](https://github.com/inbo/watina/issues/44))
  - return eight more observation well attributes beside `filterdepth`
    ([\#44](https://github.com/inbo/watina/issues/44))
  - four methods are available to aggregate observation well attributes
    per location (argument `obswell_aggr`,
    [\#44](https://github.com/inbo/watina/issues/44))
  - applying a spatial mask is now a little speedier
    ([\#57](https://github.com/inbo/watina/issues/57))
- [`as_points()`](https://inbo.github.io/watina/reference/as_points.md)
  and
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)
  return a warning when duplicated coordinates occur in their local
  result ([\#51](https://github.com/inbo/watina/issues/51), thanks
  [@w-jan](https://github.com/w-jan))
- Package dependency moved from
  [inborutils](https://inbo.github.io/inborutils) to
  [inbodb](https://inbo.github.io/inbodb): this is a change within
  [`connect_watina()`](https://inbo.github.io/watina/reference/connect_watina.md)
  which should not affect its behaviour and which reduces overall
  package dependencies.

Further, a number of smaller fixes and enhancements were made.

## watina 0.2.6 (2020-02-28)

- Redo fix
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md):
  calculation of `soilsurf_ost`
  ([\#43](https://github.com/inbo/watina/issues/43))

## watina 0.2.5 (2020-02-27)

- Bugfix in
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md):
  calculation of `soilsurf_ost`
  ([\#42](https://github.com/inbo/watina/issues/42))

## watina 0.2.4 (2020-01-29)

- Bugfix in `convertdf_enc()`
  ([\#34](https://github.com/inbo/watina/issues/34))
- Some small documentation improvements

## watina 0.2.3 (2020-01-09)

- Documentation is now generated by the newer `roxygen2` version
  `7.0.2`, resulting in a better layout of function arguments in the
  ‘usage’ section.

## watina 0.2.2 (2019-11-04)

- On Windows, the functions now convert ‘weird’ characters from the data
  warehouse to proper UTF-8.
- Bugfix ([\#29](https://github.com/inbo/watina/issues/29)) in
  [`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)
  regarding the default implementation of the `loc_validity` argument.

## watina 0.2.1 (2019-10-14)

- Fixed bug in some hyperlinks in function documentation, affecting the
  installation process on Windows (warnings were thrown).

## watina 0.2.0 (2019-10-02)

#### New features

- Several functions have been added to query and process chemical data.
- Three vignettes have been added.
- Improved various existing functionalities from version 0.1.0, based on
  user’s feedback.

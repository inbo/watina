<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3630532.svg)](https://doi.org/10.5281/zenodo.3630532)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/inbo/watina/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/watina/actions?query=workflow%3AR-CMD-check)
[![inbo r-universe-name](https://inbo.r-universe.dev/badges/:name?color=pink)](https://inbo.r-universe.dev)
[![inbo r-universe package status](https://inbo.r-universe.dev/badges/watina)](https://inbo.r-universe.dev)
<!-- badges: end -->

## Welcome

The R-package `watina` contains functions to query
and process data from the _Watina database_ at the Research Institute for
Nature and Forest (INBO).
This database is focused on groundwater data in
natural areas in Flanders (Belgium) and provides:

- groundwater level and chemical data;
- data from hydrostatic pressure sensors (mainly groundwater);
- data from atmospheric pressure sensors;
- a more limited number of surface water level and chemical data.

Currently the R package won't work outside INBO.

## Installing and using the _watina_ package

### Option 1: from r-universe repository (quickest)

To install the current package version from the `master` branch (latest stable release), run:

```r
install.packages("watina", repos = "https://inbo.r-universe.dev")
```

The above provides a pre-compiled package for Windows and macOS, which should be faster than below approaches.
INBO staff should have the INBO repository enabled already (check with `getOption("repos")`), in which case **`install.packages("watina")`** is all you need!


### Option 2, from source: installing without vignettes (quick)

```r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # as a precaution
remotes::install_github("inbo/watina", upgrade = TRUE)
```

You can consult the vignettes of the latest release on the (this) `pkgdown` [website](https://inbo.github.io/watina/): click on 'Articles' at the top.

### Option 3, from source: installing with vignettes

```r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # as a precaution
if (!("nycflights13" %in% installed.packages())) {
    install.packages("nycflights13")} # to prevent dbplyr from interrupting next step
remotes::install_github("inbo/watina",
                        build_vignettes = TRUE,
                        upgrade = TRUE)
```

Note that this will install the package from the `master` branch.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

## You are welcome to contribute!

Please have a look at our [contributing guide](.github/CONTRIBUTING.md)!


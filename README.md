<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3630532.svg)](https://doi.org/10.5281/zenodo.3630532)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Build Status](https://travis-ci.org/inbo/watina.svg?branch=master)](https://travis-ci.org/inbo/watina)
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

## Installing, testing and using the _watina_ package

```r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # as a precaution
remotes::install_github("inbo/watina",
                        build_vignettes = TRUE,
                        upgrade = TRUE)
```

Note that this will install the package from the `master` branch.
In case of an installation error because existing dependencies (already installed packages) are too old, add the argument `upgrade = TRUE`.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

## You are welcome to contribute!

Please have a look at our [contributing guide](.github/CONTRIBUTING.md)!


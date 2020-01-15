## Welcome

[![Build Status](https://travis-ci.org/inbo/watina.svg?branch=master)](https://travis-ci.org/inbo/watina)

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
remotes::install_github("inbo/watina", build_vignettes = TRUE)
```

Note that this will install the package from the `master` branch.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

## You are welcome to contribute!

Please have a look at our [contributing guide](.github/CONTRIBUTING.md)!


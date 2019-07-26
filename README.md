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
remotes::install_github("inbo/watina", 
                        build_opts = c("--no-resave-data", "--no-manual"))
```

Note that this will install the package from the `master` branch.
If you need a version from another branch, add the `ref` argument in the above function to provide the branch name.

## You are welcome to contribute!

### Coding tools: it's never too late for learning!

When writing functions for `watina`:

- please use `tidyverse`, `sf` and `raster` packages for data reading.
Discover the human-friendly way of coding a data processing pipeline through the use of [pipes](https://r4ds.had.co.nz/pipes.html)!
Organise data in R in a [tidy](https://r4ds.had.co.nz/tidy-data.html#tidy-data-1) way in order to avoid troubles later on.
Recommended resources to get started are:
    - [R for Data Science](https://r4ds.had.co.nz/)
    - [Geocomputation with R](https://geocompr.robinlovelace.net)
    - [R packages](http://r-pkgs.had.co.nz/) (by Hadley Wickham 2015; an extended/updated [version](https://r-pkgs.org/) is still under development)
    - `vignette("formatting", package = "roxygen2")` for documentation syntax
- have a quick look at the [tidyverse style guide](https://style.tidyverse.org/).
There you see how to style object, variable and function names, as well as the documentation.
At least keep in mind: **use lower case and 'snake_case'** for object, variable and function names.

If your function returns data:

- return data in a [tidy](https://r4ds.had.co.nz/tidy-data.html#tidy-data-1) way.
- use `dplyr::as_tibble()` to return a dataframe as a tibble.
A tibble is a dataframe that makes working in the tidyverse a little [easier](https://r4ds.had.co.nz/tibbles.html).
- return data as much as possible internationalized: use English names for dataframe variables!


### How can I contribute code?

More detailed info on git workflows at INBO: <https://inbo.github.io/tutorials/tags/git/>.
See also [these git workshop materials](https://inbo.github.io/git-course/index.html).

1. Make commits (in your local clone of the remote repo on Github) _in your own git branch_, branched off from the `master` branch.
(But see this in a relative manner: exactly the same process can be repeated by someone else in turn, relative to your branch.
So '`master`' in this protocol can be replaced by another branch name!)
You can push your branch to the remote as often as you like, as it will not influence other branches (first time: do `git push -u origin yourbranchname`; afterwards `git push` suffices). It serves as a backup and enables others to work with you on that branch.
1. Meanwhile, make sure that your branch stays up to date with evolutions in `master` (i.e. in your local repo, update `master` with `git checkout master && git pull` and then, with your own branch checked out again, do `git merge --no-ff master`), in order to prevent merge conflicts with `master` later on.
At this stage, you need to resolve any merge conflicts that may arise in your own branch.
1. Propose to merge your commits into `master`: this starts with making a 'pull request' (PR; actually this is a merge request) and assign at least one reviewer before a merge can be decided. At that moment, open online discussion in the repo is possible on your changes (for other open discussion that you want to start, make an _issue_). As long as no merge is performed, more commits can be added to this PR with `git push`, e.g. to implement requested changes by others.
    - note that, if you branched off another (reference) branch than `master`, make sure to change the reference branch in the pull request (the default reference is `master`).
1. After your PR is merged, pull the reference branch (usually `master`) and clean up your local repo in order to keep up with the remote.

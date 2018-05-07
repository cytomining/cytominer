[![Build Status](https://travis-ci.org/cytomining/cytominer.png?branch=master)](https://travis-ci.org/cytomining/cytominer) [![Coverage Status](https://img.shields.io/codecov/c/github/cytomining/cytominer/master.svg)](https://codecov.io/github/cytomining/cytominer?branch=master) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/cytominer)](https://cran.r-project.org/package=cytominer)

cytominer
=========

Typical morphological profiling datasets have millions of cells and hundreds of features per cell. When working with this data, you must

-   clean the data

-   normalize the features so that they are comparable across experiments

-   transform the features so that their distributions are well-behaved ( i.e., bring them in line with assumptions we want to make about their disributions)

-   select features based on their quality

-   aggregate the single-cell data, if needed

The cytominer package makes these steps fast and easy.

Installation
------------

You can install `cytominer` from CRAN:

``` r
install.packages("cytominer")
```

Or, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("cytomining/cytominer", dependencies = TRUE, build_vignettes = TRUE)
```

Occasionally, the `Suggests` dependencies [may not get installed](https://github.com/hadley/devtools/issues/1370), depending on your system, so you'd need to install those explicitly.

Example
-------

See `vignette("cytominer-pipeline")` for basic example of using cytominer to analyze a morphological profiling dataset.


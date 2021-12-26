
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecodown

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/edgararuiz/ecodown/branch/main/graph/badge.svg)](https://app.codecov.io/gh/edgararuiz/ecodown?branch=main)
[![R-CMD-check](https://github.com/edgararuiz/ecodown/workflows/R-CMD-check/badge.svg)](https://github.com/edgararuiz/ecodown/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ecodown)](https://CRAN.R-project.org/package=ecodown)
<!-- badges: end -->

The goal of `ecodown` is to make it possible for your R package’s
documentation to be published in a Quarto site.

The vision for `ecodown` is that it is used to document a group of
related packages that will be published to a single Quarto site.

## Installation

You can install the development version of `ecodown` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/ecodown")
```

## Using

``` r
library(ecodown)
```

     ├── _ecodown.yml
     ├── _quarto.yml
     └── index.md

Contents of example ’\_ecodown.yml’ file:

    site:
      packages:
        - repo_url: https://github.com/rstudio/mleap
          commit: 39267f6a253f51d1176e58e528c7ce2c6cfd2a64
        - repo_url: https://github.com/rstudio/graphframes
          convert_readme: FALSE

``` r
ecodown_build()
```

           Clone / Checkout       | R N Art Ref |
    mleap (39267f6...)            | 1 1   0    7|
    graphframes (v0.1.2)          | 0 1   0   27|


    >> Render in Quarto
    my_site (1 renderable file)
    |--- graphframes (1 renderable file)
    |--- |--- reference (27 renderable files)
    |--- mleap (2 renderable files)
    |--- |--- reference (7 renderable files)
    ============================================== 
    Total files:  38 

    >> Autolinking
    docs (1 html file)
    |--- graphframes (1 html file)
    |--- |--- reference (27 html files)
    |--- mleap (2 html files)
    |--- |--- reference (7 html files)
    ============================================== 
    Total files:  38 

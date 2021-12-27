
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

## Clone the repo

``` r
mleap_location <- ecodown_clone("https://github.com/rstudio/mleap")
>> Cloning repo
- Cloning: mleap
- Checking out tag: v1.0.0
```

## Convert and move to Quarto folder

``` r
ecodown_convert(mleap_location, quarto_sub_folder = "mleap")
```

    mleap (2 files)
    |--- README.md [ 0s ]
    |--- NEWS.md [ 0s ]
    |--- man (7 files)
    |--- |--- install_maven.Rd [ 0s ]
    |--- |--- install_mleap.Rd [ 0s ]
    |--- |--- ml_write_bundle.Rd [ 0s ]
    |--- |--- mleap_installed_versions.Rd [ 0s ]
    |--- |--- mleap_load_bundle.Rd [ 0s ]
    |--- |--- mleap_model_schema.Rd [ 0s ]
    |--- |--- mleap_transform.Rd [ 0s ]
    ============================================== 
    Total files:  9  ---- Total time:  0s 

## Render site

``` r
ecodown_quarto_render()
```

    >> Render Quarto site
    my_site (1 renderable file)
    |--- index.md [ 0.9s ]
    |--- mleap (2 renderable files)
    |--- |--- index.md [ 1s ]
    |--- |--- news.md [ 0.9s ]
    |--- |--- reference (8 renderable files)
    |--- |--- |--- index.md [ 0.9s ]
    |--- |--- |--- install_maven.md [ 1s ]
    |--- |--- |--- install_mleap.md [ 1s ]
    |--- |--- |--- ml_write_bundle.md [ 1s ]
    |--- |--- |--- mleap_installed_versions.md [ 1s ]
    |--- |--- |--- mleap_load_bundle.md [ 1s ]
    |--- |--- |--- mleap_model_schema.md [ 1s ]
    |--- |--- |--- mleap_transform.md [ 1s ]
    ============================================== 
    Total files:  11  ---- Total time:  10.8s 

## Autolink

``` r
ecodown_autolink()
```

    >> Auto-linking
    - Path: /var/folders/l8/v1ym1mc10_b0dftql5wrrm8w0000gn/T/RtmpPhuIAs/my_site/docs
    docs (1 html file)
    |--- index.html [ 0s ]
    |--- mleap (2 html files)
    |--- |--- index.html [ 2.4s ]
    |--- |--- news.html [ 0s ]
    |--- |--- reference (8 html files)
    |--- |--- |--- index.html [ 0s ]
    |--- |--- |--- install_maven.html [ 0.1s ]
    |--- |--- |--- install_mleap.html [ 0s ]
    |--- |--- |--- ml_write_bundle.html [ 0s ]
    |--- |--- |--- mleap_installed_versions.html [ 0s ]
    |--- |--- |--- mleap_load_bundle.html [ 0s ]
    |--- |--- |--- mleap_model_schema.html [ 0s ]
    |--- |--- |--- mleap_transform.html [ 0s ]
    ============================================== 
    Total files:  11  ---- Total time:  2.6s 

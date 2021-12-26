
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
    |--- README.md
    |--- NEWS.md
    |--- man (7 files)
    |--- |--- install_maven.Rd
    |--- |--- install_mleap.Rd
    |--- |--- ml_write_bundle.Rd
    |--- |--- mleap_installed_versions.Rd
    |--- |--- mleap_load_bundle.Rd
    |--- |--- mleap_model_schema.Rd
    |--- |--- mleap_transform.Rd
    ============================================== 
    Total files:  9 

## Render site

``` r
ecodown_quarto_render()
```

    >> Render Quarto site
    my_site (1 renderable file)
    |--- index.md
    |--- mleap (2 renderable files)
    |--- |--- index.md
    |--- |--- news.md
    |--- |--- reference (8 renderable files)
    |--- |--- |--- index.md
    |--- |--- |--- install_maven.md
    |--- |--- |--- install_mleap.md
    |--- |--- |--- ml_write_bundle.md
    |--- |--- |--- mleap_installed_versions.md
    |--- |--- |--- mleap_load_bundle.md
    |--- |--- |--- mleap_model_schema.md
    |--- |--- |--- mleap_transform.md
    ============================================== 
    Total files:  11 

## Autolink

``` r
ecodown_autolink()
```

    >> Auto-linking
    - Path: /var/folders/l8/v1ym1mc10_b0dftql5wrrm8w0000gn/T/RtmpjhlgVG/my_site/docs
    docs (1 html file)
    |--- index.html
    |--- mleap (2 html files)
    |--- |--- index.html
    |--- |--- news.html
    |--- |--- reference (8 html files)
    |--- |--- |--- index.html
    |--- |--- |--- install_maven.html
    |--- |--- |--- install_mleap.html
    |--- |--- |--- ml_write_bundle.html
    |--- |--- |--- mleap_installed_versions.html
    |--- |--- |--- mleap_load_bundle.html
    |--- |--- |--- mleap_model_schema.html
    |--- |--- |--- mleap_transform.html
    ============================================== 
    Total files:  11 

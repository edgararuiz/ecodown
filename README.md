
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecodown

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/edgararuiz/ecodown/branch/main/graph/badge.svg)](https://app.codecov.io/gh/edgararuiz/ecodown?branch=main)
<!-- badges: end -->

The goal of `ecodown` is to help convert package documentation to
Quarto. The main idea is to use `ecodown` to document multiple packages
in a single Quarto site.

## Installation

You can install the development version of ecodown from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/ecodown")
```

## Example

``` r
library(ecodown)
```

### Clone the package’s repo

`ecodown` uses the package’s source to produce the documentation. Use
`package_clone_git_repo()` to download the the package. By default, it
clones the repo to a temp directory. We will use the `mleap` package for
this example:

``` r
mleap_location <- package_clone_git_repo("https://github.com/rstudio/mleap")
```

Here are the current contents of the package:

    #>  ├── DESCRIPTION
    #>  ├── LICENSE.md
    #>  ├── NAMESPACE
    #>  ├── NEWS.md
    #>  ├── R
    #>  ├── README.Rmd
    #>  ├── README.md
    #>  ├── codecov.yml
    #>  ├── configure.R
    #>  ├── cran-comments.md
    #>  ├── inst
    #>  ├── java
    #>  ├── man
    #>  ├── mleap.Rproj
    #>  └── tests

And the contents of the ‘man’ folder:

    #>  ├── install_maven.Rd
    #>  ├── install_mleap.Rd
    #>  ├── ml_write_bundle.Rd
    #>  ├── mleap_installed_versions.Rd
    #>  ├── mleap_load_bundle.Rd
    #>  ├── mleap_model_schema.Rd
    #>  └── mleap_transform.Rd

### Build the documentation

`package_build_documentation()` is the main function out of `ecodown`.
It is similar to the `pkgdown`‘s `build_site()`. It will map and copy
the README, NEWS and vignettes files. It will also parse and convert
the’.Rd’ files into ‘.Md’ files.

The `project_folder` creates a sub-folder in your workspace where the
documentation is going to be converted and copied to.

``` r
package_build_documentation(
  pkg_folder = mleap_location,
  project_folder = "mleap"
)
```

    #> - - - - - - - - Top files - - - - - - - - -
    #> - Copied: mleap/index.md
    #> - Copied: mleap/news.md
    #> - - - - - - - Article files - - - - - - - -
    #> - Vignette folder not found
    #> - - - - - - - Reference files - - - - - - -
    #> - Created: mleap/reference/index.md
    #> - Created: mleap/reference/install_maven.md
    #> - Created: mleap/reference/install_mleap.md
    #> - Created: mleap/reference/ml_write_bundle.md
    #> - Created: mleap/reference/mleap_installed_versions.md
    #> - Created: mleap/reference/mleap_load_bundle.md
    #> - Created: mleap/reference/mleap_model_schema.md
    #> - Created: mleap/reference/mleap_transform.md

This will be the contents of the new ‘mleap’ sub-folder.

    #>  ├── index.md
    #>  ├── news.md
    #>  └── reference
    #>      ├── index.md
    #>      ├── install_maven.md
    #>      ├── install_mleap.md
    #>      ├── ml_write_bundle.md
    #>      ├── mleap_installed_versions.md
    #>      ├── mleap_load_bundle.md
    #>      ├── mleap_model_schema.md
    #>      └── mleap_transform.md


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

In general, there are 5 steps to keep the documents up-to-date in
Quarto:

1.  [Download the latest package
    source](#download-the-latest-package-source)
2.  [Prepare the documentation for
    Quarto](#prepare-the-documentation-for-quarto)
3.  [Run Quarto locally](#run-quarto-locally)
4.  [Run auto-linking](#run-auto-linking)
5.  Commit the changes back to the site

``` r
library(ecodown)
```

### Download the latest package source

The assumption is that the Git repository of the package is different
from that of the Quarto site. Again, the idea is that the Quarto site is
being used to document multiple packages.

This means that the package’s source will be external to the Quarto
site’s repository, so it is likely that the latest bits will have to be
cloned from Git.

`ecodown` uses the package’s source to produce the documentation. Use
`package_clone_git_repo()` to download the the package. By default, it
clones the repo to a temp directory. We will use the `mleap` package for
this example:

``` r
mleap_location <- package_clone_git_repo("https://github.com/rstudio/mleap")
```

This is not a required step, only use if the latest source is not
available in your laptop, or if you are using `ecodown` as part of your
CI/CD automation.

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

And the contents of the **man** folder:

    #>  ├── install_maven.Rd
    #>  ├── install_mleap.Rd
    #>  ├── ml_write_bundle.Rd
    #>  ├── mleap_installed_versions.Rd
    #>  ├── mleap_load_bundle.Rd
    #>  ├── mleap_model_schema.Rd
    #>  └── mleap_transform.Rd

### Prepare the documentation for Quarto

`package_build_documentation()` is the main function out of `ecodown`.
It is similar to the `pkgdown`’s `build_site()`. It will map and copy
the README, NEWS and vignettes files. It will also parse and convert the
**.Rd** files into **.Md** files.

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
    #> - Copied: mleap/NEWS.md
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

## Run Quarto locally

Use `quarto::quarto_render()` to convert the markdown files into HTML.

For this example, an ‘index.md’ and a ’\_quarto.yml’ file to make it
possible to run Quarto properly. This particular setup file points
Quarto to a **docs** sub-folder.

``` r
quarto::quarto_render()
```

This the content of the **docs** sub-folder after the process completes:

    #>  ├── index.html
    #>  ├── mleap
    #>  │   ├── NEWS.html
    #>  │   ├── index.html
    #>  │   └── reference
    #>  │       ├── index.html
    #>  │       ├── install_maven.html
    #>  │       ├── install_mleap.html
    #>  │       ├── ml_write_bundle.html
    #>  │       ├── mleap_installed_versions.html
    #>  │       ├── mleap_load_bundle.html
    #>  │       ├── mleap_model_schema.html
    #>  │       └── mleap_transform.html
    #>  ├── search.json
    #>  ... more files ...

## Run auto-linking

Auto-linking here refers to converting written function names in a given
HTML to links to the reference of that function. It uses the package
`downlit` to accomplish this.

In the background, `package_build_documentation()` not only builds the
documentation in a format Quarto accepts, but also pre-loads the
necessary R `options` so that `downlit` is aware that your package is
inside your Quarto site. This will enable `downlit` to link references
of your package to your Quarto site, instead of creating a generic link.

Use the `site_autolink_html()` function to perform the auto-link over
all of the Quarto site, or a sub-folder in it.

``` r
site_autolink_html("docs")
```

    #> - - - - - - - Auto-linking - - - - - - - - -
    #> - Processed: index.html
    #> - Processed: NEWS.html
    #> - Processed: index.html
    #> - Processed: index.html
    #> - Processed: install_maven.html
    #> - Processed: install_mleap.html
    #> - Processed: ml_write_bundle.html
    #> - Processed: mleap_installed_versions.html
    #> - Processed: mleap_load_bundle.html
    #> - Processed: mleap_model_schema.html
    #> - Processed: mleap_transform.html

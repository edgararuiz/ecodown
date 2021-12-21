
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
site_build_quarto()
```

    - - - - - - - - - - - - - - Package documentation - - - - - - - - - - - - - - 
    - quarto_sub_folder: mleap
    - site_url: https://www.mysite.com/
    - - - - - - - - - - - - - - - - - Cloning repo - - - - - - - - - - - - - - - -
    - Cloning: mleap
    - Checking out SHA: 39267f6...
    - - - - - - - - - - - - - - - - - Top files - - - - - - - - - - - - - - - - - 
    - Copied: mleap/index.md
    - Copied: mleap/NEWS.md
    - - - - - - - - - - - - - - - - Article files - - - - - - - - - - - - - - - - 
    - Vignette folder not found
    - - - - - - - - - - - - - - - - Reference files - - - - - - - - - - - - - - - 
    - Created: mleap/reference/index.md
    - Created: mleap/reference/install_maven.md
    - Created: mleap/reference/install_mleap.md
    - Created: mleap/reference/ml_write_bundle.md
    - Created: mleap/reference/mleap_installed_versions.md
    - Created: mleap/reference/mleap_load_bundle.md
    - Created: mleap/reference/mleap_model_schema.md
    - Created: mleap/reference/mleap_transform.md
    - - - - - - - - - - - - - - Package documentation - - - - - - - - - - - - - - 
    - quarto_sub_folder: graphframes
    - site_url: https://www.mysite.com/
    - - - - - - - - - - - - - - - - - Cloning repo - - - - - - - - - - - - - - - -
    - Cloning: graphframes
    - Checking out tag: v0.1.2
    - - - - - - - - - - - - - - - - - Top files - - - - - - - - - - - - - - - - - 
    - Copied: graphframes/NEWS.md
    - - - - - - - - - - - - - - - - Article files - - - - - - - - - - - - - - - - 
    - Vignette folder not found
    - - - - - - - - - - - - - - - - Reference files - - - - - - - - - - - - - - - 
    - Created: graphframes/reference/index.md
    - Created: graphframes/reference/gf_bfs.md
    - Created: graphframes/reference/gf_cache.md
    - Created: graphframes/reference/gf_chain.md
    - Created: graphframes/reference/gf_connected_components.md
    - Created: graphframes/reference/gf_degrees.md
    - Created: graphframes/reference/gf_edge_columns.md
    - Created: graphframes/reference/gf_edges.md
    - Created: graphframes/reference/gf_find.md
    - Created: graphframes/reference/gf_friends.md
    - Created: graphframes/reference/gf_graphframe.md
    - Created: graphframes/reference/gf_grid_ising_model.md
    - Created: graphframes/reference/gf_in_degrees.md
    - Created: graphframes/reference/gf_lpa.md
    - Created: graphframes/reference/gf_out_degrees.md
    - Created: graphframes/reference/gf_pagerank.md
    - Created: graphframes/reference/gf_persist.md
    - Created: graphframes/reference/gf_register.md
    - Created: graphframes/reference/gf_scc.md
    - Created: graphframes/reference/gf_shortest_paths.md
    - Created: graphframes/reference/gf_star.md
    - Created: graphframes/reference/gf_triangle_count.md
    - Created: graphframes/reference/gf_triplets.md
    - Created: graphframes/reference/gf_two_blobs.md
    - Created: graphframes/reference/gf_unpersist.md
    - Created: graphframes/reference/gf_vertex_columns.md
    - Created: graphframes/reference/gf_vertices.md
    - Created: graphframes/reference/spark_graphframe.md
    - - - - - - - - - - - - - - - Render Quarto site - - - - - - - - - - - - - - -
    [ 1/40] graphframes/NEWS.md
    [ 2/40] graphframes/reference/gf_triangle_count.md
    [ 3/40] graphframes/reference/gf_lpa.md
    [ 4/40] graphframes/reference/gf_pagerank.md
    [ 5/40] graphframes/reference/spark_graphframe.md
    [ 6/40] graphframes/reference/gf_vertices.md
    [ 7/40] graphframes/reference/gf_edge_columns.md
    [ 8/40] graphframes/reference/gf_shortest_paths.md
    [ 9/40] graphframes/reference/gf_in_degrees.md
    [10/40] graphframes/reference/gf_find.md
    [11/40] graphframes/reference/gf_graphframe.md
    [12/40] graphframes/reference/gf_friends.md
    [13/40] graphframes/reference/gf_bfs.md
    [14/40] graphframes/reference/gf_unpersist.md
    [15/40] graphframes/reference/gf_edges.md
    [16/40] graphframes/reference/gf_cache.md
    [17/40] graphframes/reference/gf_grid_ising_model.md
    [18/40] graphframes/reference/index.md
    [19/40] graphframes/reference/gf_out_degrees.md
    [20/40] graphframes/reference/gf_vertex_columns.md
    [21/40] graphframes/reference/gf_scc.md
    [22/40] graphframes/reference/gf_chain.md
    [23/40] graphframes/reference/gf_star.md
    [24/40] graphframes/reference/gf_register.md
    [25/40] graphframes/reference/gf_two_blobs.md
    [26/40] graphframes/reference/gf_triplets.md
    [27/40] graphframes/reference/gf_degrees.md
    [28/40] graphframes/reference/gf_persist.md
    [29/40] graphframes/reference/gf_connected_components.md
    [30/40] index.md
    [31/40] mleap/NEWS.md
    [32/40] mleap/index.md
    [33/40] mleap/reference/mleap_installed_versions.md
    [34/40] mleap/reference/install_mleap.md
    [35/40] mleap/reference/install_maven.md
    [36/40] mleap/reference/ml_write_bundle.md
    [37/40] mleap/reference/index.md
    [38/40] mleap/reference/mleap_transform.md
    [39/40] mleap/reference/mleap_load_bundle.md
    [40/40] mleap/reference/mleap_model_schema.md

    Output created: docs/index.html

    - - - - - - - - - - - - - - - - - Auto-linking - - - - - - - - - - - - - - - -
    - Path: /var/folders/l8/v1ym1mc10_b0dftql5wrrm8w0000gn/T/Rtmpjmo9OV/test_site2/docs
    - Processed: NEWS.html
    - Processed: gf_bfs.html
    - Processed: gf_cache.html
    - Processed: gf_chain.html
    - Processed: gf_connected_components.html
    - Processed: gf_degrees.html
    - Processed: gf_edge_columns.html
    - Processed: gf_edges.html
    - Processed: gf_find.html
    - Processed: gf_friends.html
    - Processed: gf_graphframe.html
    - Processed: gf_grid_ising_model.html
    - Processed: gf_in_degrees.html
    - Processed: gf_lpa.html
    - Processed: gf_out_degrees.html
    - Processed: gf_pagerank.html
    - Processed: gf_persist.html
    - Processed: gf_register.html
    - Processed: gf_scc.html
    - Processed: gf_shortest_paths.html
    - Processed: gf_star.html
    - Processed: gf_triangle_count.html
    - Processed: gf_triplets.html
    - Processed: gf_two_blobs.html
    - Processed: gf_unpersist.html
    - Processed: gf_vertex_columns.html
    - Processed: gf_vertices.html
    - Processed: index.html
    - Processed: spark_graphframe.html
    - Processed: index.html
    - Processed: NEWS.html
    - Processed: index.html
    - Processed: index.html
    - Processed: install_maven.html
    - Processed: install_mleap.html
    - Processed: ml_write_bundle.html
    - Processed: mleap_installed_versions.html
    - Processed: mleap_load_bundle.html
    - Processed: mleap_model_schema.html
    - Processed: mleap_transform.html
    - - - - - - - - - - - - - - - - - - Complete - - - - - - - - - - - - - - - - -

---
title: "Clones repo and builds documentation"
---

*R/clone-convert.R*

## ecodown_clone_convert

## Description
 Clones repo and builds documentation 


## Usage
```r
 
ecodown_clone_convert( 
  repo_url = "", 
  quarto_sub_folder = path_file(repo_url), 
  quarto_folder = here::here(), 
  version_folder = "", 
  convert_readme = TRUE, 
  convert_news = TRUE, 
  convert_articles = TRUE, 
  convert_reference = TRUE, 
  reference_folder = "reference", 
  vignettes_folder = "articles", 
  downlit_options = TRUE, 
  site_url = qe(quarto_folder, "site", "site-url"), 
  commit = c("latest_tag", "latest_commit"), 
  target_folder = tempdir(), 
  branch = "main", 
  reference_examples = TRUE, 
  reference_examples_not_run = FALSE, 
  verbosity = c("verbose", "summary", "silent"), 
  reference_output = "qmd", 
  reference_qmd_options = NULL, 
  reference_template = NULL, 
  package_description = NULL, 
  versions = list() 
) 
```

## Arguments
|Arguments|Description|
|---|---|
| repo_url | Repo location |
| quarto_sub_folder | Sub folder in `quarto_folder` that will be the base for the package's documentation. |
| quarto_folder | Base target Quarto folder. Defaults to current workspace. |
| version_folder | Folder path to save the documentation version. Examples are: "latest", "dev", "v1.0". Defaults to empty. |
| convert_readme | Flag that indicates if the README file needs to be processed |
| convert_news | Flag that indicates if the NEWS file needs to be processed |
| convert_articles | Flag that indicates if the vignette files needs to be processed |
| convert_reference | Flag that indicates if the help files needs to be processed |
| reference_folder | The sub folder where the reference files will be placed. Defaults to "reference". |
| vignettes_folder | The sub folder where the vignette files will be placed. Defaults to "articles". |
| downlit_options | Flag that indicates if the package name should be added to the 'options()' that tells 'downlit' that this is an internal package |
| site_url | URL of the target site.  It defaults to using the address in the '_quarto.yml' file |
| commit | Commit to use as the base for the documentation.  It defaults to 'latest_tag'. That default will search for the latest Git tag.  The assumption is that the latest tag is the same as the latest release.  This way we avoid documenting work-in-progress.  The 'latest_commit' value will simply use whatever is cloned. Pass an SHA value if you wish to fix the commit to use. |
| target_folder | Location to copy the package to. Defaults to a temporary directory |
| branch | Repo branch. Defaults to 'main' |
| reference_examples | Boolean flag to indicate if the Examples inside the reference page is to be evaluated. |
| reference_examples_not_run | Boolean flag to indicate if the Examples marked as DO NOT RUN is to be ignored and the code should be evaluated. |
| verbosity | Level of messaging available during run time. Possible values are 'verbose', 'summary', and 'silent'. |
| reference_output | File type for all the reference files. Possible options are `qmd` and `md`. Defaults to `qmd`. |
| reference_qmd_options | A character variable that contains the text version of additions to the reference front matter. It applies only to when reference output is `qmd` |
| reference_template | The path to a `.qmd` file to use as the template to create the reference pages. |
| package_description | Custom description for the package. If NULL then the package's Title, inside DESCRIPTION, will be used. |
| versions | A list of additional reference versions to convert. Pass the commit to use, the name of the version, and the sub-folder to use. It expects a named list object. Example: list(list(commit = "latest_commit", path = "dev")) |








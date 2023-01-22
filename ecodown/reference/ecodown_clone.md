---
title: "Download the package's latest source code from repo"
---

*R/clone.R*

## ecodown_clone

## Description
 Download the package's latest source code from repo 


## Usage
```r
 
ecodown_clone( 
  repo_url = "", 
  target_folder = tempdir(), 
  branch = "main", 
  verbosity = c("verbose", "summary", "silent") 
) 
```

## Arguments
|Arguments|Description|
|---|---|
| repo_url | Repo location |
| target_folder | Location to copy the package to. Defaults to a temporary directory |
| branch | Repo branch. Defaults to 'main' |
| verbosity | Level of messaging available during run time. Possible values are 'verbose', 'summary', and 'silent'. |





## Examples
```{r, eval=ecodown::examples_run()}
library(ecodown)
 
pkg_path <- ecodown_clone("https://github.com/tidyverse/hms") 
list.files(pkg_path) 
```



---
title: "Autolink function calls"
---

*R/autolink.R*

## ecodown_autolink

## Description
 Autolink function calls 


## Usage
```r
 
ecodown_autolink( 
  quarto_folder = here::here(), 
  render_folder = qe(quarto_folder, "project", "output-dir"), 
  verbosity = c("verbose", "summary", "silent") 
) 
```

## Arguments
|Arguments|Description|
|---|---|
| quarto_folder | Base target Quarto folder. Defaults to current workspace. |
| render_folder | Location of the sub-folder that contains the output from Quarto. It defaults to the 'output-dir' entry in the '_quarto.yml' file. |
| verbosity | Level of messaging available during run time. Possible values are 'verbose', 'summary', and 'silent'. |








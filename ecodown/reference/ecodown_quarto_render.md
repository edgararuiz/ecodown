---
title: "Wrapper on the Quarto Render function"
---

*R/quarto.R*

## ecodown_quarto_render

## Description
 Wrapper on the Quarto Render function 


## Usage
```r
 
ecodown_quarto_render( 
  quarto_folder = here::here(), 
  verbosity = c("verbose", "summary", "silent"), 
  autolink = FALSE, 
  run = c("smart", "full", "no") 
) 
```

## Arguments
|Arguments|Description|
|---|---|
| quarto_folder | Base target Quarto folder. Defaults to current workspace. |
| verbosity | Level of messaging available during run time. Possible values are 'verbose', 'summary', and 'silent'. |
| autolink | Boolean flag that indicates if 'downlit' should run. Defaults to TRUE. |
| run | Flag that indicates the mode to render the site. Defaults to 'smart' which attempts to run only the files that changed. |








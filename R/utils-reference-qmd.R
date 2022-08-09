reference_qmd <- function(file_in, pkg, output_options = NULL) {
  parsed <- reference_parse_qmd(file_in, pkg)
  
  new <- parsed$section

  writeLines(
    c("---", "  ", "---", as.character(new)),
    "test.qmd"
  )  
}

reference_parse_qmd <- function(file_in, pkg) {
  
  parsed <- reference_parse("here.Rd", pkgdown::as_pkgdown("../ecodown-test/_packages/here"))
  
  res <- purrr::imap(parsed, ~ {
    res <- NULL
    
    if(.y == "examples") {
      res <- map(.x, reference_qmd_example, FALSE)
    } 
    
    if(is.null(res)) {
      res <- map(.x, ~ reduce(.x, function(x, y) c(x, "", "", y)))
    }
    
    
    res
  })
  map(res, ~ reduce(.x, function(x, y) c(x, "", y)))
}

reference_qmd_generic <- function(x, title = NULL) {
  if (!is.null(x)) {
    c(
      pate("##", title),
      reference_tag(x)
    )
  } else {
    ""
  }
}

reference_qmd_example <- function(x, run = FALSE) {
  if(run) {
    out <- c("```{r}",  x, "```")
  } else {
    out <- c("```r",  x, "```")
  }
}
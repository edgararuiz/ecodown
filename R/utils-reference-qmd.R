reference_qmd <- function(file_in, pkg, output_options = NULL) {
  parsed <- reference_content_default(file_in, pkg)

  writeLines(
    c("---", "  ", "---", as.character(parsed)),
    "test.qmd"
  )  
  quarto::quarto_render("test.qmd")
}

reference_content_default <- function(file_in, pkg) {
  #parsed <- reference_to_list_page("here.Rd", pkgdown::as_pkgdown("../ecodown-test/_packages/here"))
  #parsed <- reference_to_list_page("ml_bisecting_kmeans.Rd", pkgdown::as_pkgdown("../sparklyr"))
  parsed <- reference_to_list_page(file_in, pkg)
  con <- reference_convert(parsed)
  
  out <- c(
    reference_entry(paste("##", con$name)), 
    reference_entry(con$title),
    reference_entry(con$description, "Description"),
    reference_entry(con$usage, "Usage"),
    reference_entry(con$arguments, "Arguments"),
    reference_entry(con$details, "Details"),
    reference_entry(con$section),
    reference_entry(con$value, "Value"),
    reference_entry(con$examples, "Examples"),
    reference_entry(con$seealso, "See Also")
  )
  
  as.character(out)
}

reference_entry <- function(x, title = NULL) {
  out <- NULL
  if(!is.null(title)) title <- paste("##", title)
  if(!is.null(x)) out <- c(title, "", x, "")
  out
}

reference_convert <- function(x) {
  res <- list()
  for(i in seq_along(x)) {
    curr <- x[[i]]
    curr_name <- names(x[i])
    out <- NULL
    
    if(curr_name == "examples") {
      out <- map(curr, reference_qmd_example, FALSE)
      out <- flatten(out)
    }
    
    if(curr_name == "usage") {
      out <- reference_qmd_example(curr, FALSE)
    }
    
    if(curr_name == "arguments") out <- reference_arguments(curr)
      
    if(curr_name == "section") {
      out <- curr %>% 
        map(~ c(paste("##", .x$title), .x$contents)) %>% 
        flatten() %>% 
        reduce(function(x, y) c(x, "", y), .init = NULL)
    }
    
    if(is.null(out)) {
      out <- curr
      if(is.list(out)) out <- flatten(out)
      out <- reduce(out, function(x, y) c(x, "", y), .init = NULL)
    }

    out <- list(out)  
    names(out) <- curr_name
    
    res <- c(res, out)
  }
  
  res  
  
}

reference_arguments <- function(x) {
  lines <- map_chr(x, ~ paste0(.x[[1]], " | ", .x[[2]]))
  rows <- paste0("| ", lines, " |")
  c("|Arguments|Description|", "|---|---|", rows)
}


reference_qmd_example <- function(x, run = FALSE) {
  #x <- x[x != "\n"]
  if(run) {
    out <- c("```{r}",  x, "```")
  } else {
    out <- c("```r",  x, "```")
  }
}

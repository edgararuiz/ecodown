reference_to_qmd<- function(file_in, pkg, template = NULL) {
  
  parsed <- reference_to_list_page(file_in, pkg)
  con <- reference_convert(parsed)
  
  if(is.null(template)) {
    template_path <- system.file("templates/reference.qmd", package = "ecodown")  
  } else {
    template_path <- template
  }
  
  
  template <- readLines(template_path)
  
  template %>% 
    map(parse_line_tag, con) %>% 
    flatten() %>% 
    as.character()
}

parse_line_tag <- function(line, con) {
  start_tag <- "\\{\\{\\{\\{"
  end_tag <- "\\}\\}\\}\\}"
  
  tr <- c(
    "seealso" = "See Also", 
    "author" = "Author(s)"
  )
  
  if(grepl(start_tag, line)) {
    start_half <- strsplit(line, start_tag)[[1]]
    end_half <- strsplit(start_half[[2]], end_tag)[[1]]
    tag_full <-strsplit(end_half[[1]], "\\.")[[1]]
    tag_title <- tag_full[[1]]
    tag_name <- tag_full[[2]]
    
    tag_name_label <- paste0(
      toupper(substr(tag_name, 1, 1)), 
      substr(tag_name, 2, nchar(tag_name))
      )
    
    tag_match <- names(tr) == tag_name
    
    if(any(tag_match)) {
      tag_name_label <- tr[tag_match]
    }
    
    start_with <- NULL 
    if(length(start_half) > 1) start_with <- start_half[[1]]
    end_with <- NULL
    if(length(end_half) > 1) end_with <- end_half[[2]]
    
    start_with <- start_with[start_with != ""]
    end_with <- end_with[end_with != ""]
    out <- NULL
    tag_content <- con[[tag_name]]
    if(!is.null(tag_content)) {
      if(tag_title == "title") {
        tag_content <- c(paste0("## ", tag_name_label), tag_content)
      } 
      if(length(tag_content) == 1) {
        out <- paste0(start_with, tag_content, end_with)
      } else {
        out <- c(start_with, tag_content, end_with)  
      }
        
    }
    out
  } else {
    line
  }
} 

reference_content_default <- function(file_in, 
                                      pkg, 
                                      output = "qmd", 
                                      output_options = "",
                                      examples = FALSE,
                                      examples_not_run = FALSE
                                      ) {
  parsed <- reference_to_list_page(file_in, pkg)
  con <- reference_convert(parsed)
  alias <- paste("#", con$alias)
  
  
  
  out <- c(
    alias, 
    reference_entry(con$title),
    reference_entry(con$description, "Description"),
    reference_entry(con$format, "Format"),
    reference_entry(con$usage, "Usage"),
    reference_entry(con$arguments, "Arguments"),
    reference_entry(con$details, "Details"),
    reference_entry(con$section),
    reference_entry(con$value, "Value"),
    reference_entry(con$note, "Note"),
    reference_entry(con$examples, "Examples"),
    reference_entry(con$seealso, "See Also"),
    reference_entry(con$author, "Author(s)")
  )
  
  if(output == "qmd") {
    source <- paste0("  script: ", con$source)
    repo <-  paste0("  repo: ", con$repo)
    source_link <- path(con$repo, "blob/main", con$source)
    out <- c("---", 
             output_options, 
             "source:",
             source,
             repo,
             "---", 
             "",
             paste0("[View source on GitHub](", source_link,")"),
             "",
             out
             )
  } 
  
  as.character(out)
}

reference_entry <- function(x, title = NULL) {
  out <- NULL
  if(!is.null(title)) title <- paste("##", title)
  if(!is.null(x)) out <- c(title, "", x, "")
  out
}

reference_convert <- function(x, output = "qmd") {
  res <- list()
  for(i in seq_along(x)) {
    curr <- x[[i]]
    curr_name <- names(x[i])
    out <- NULL
    
    if(curr_name == "examples") {
      run_examples <- FALSE
      if(output == "md") {
        out <- map(curr, reference_qmd_example, FALSE)
        out <- flatten(out)
      } else {
        out <- list()
        if(!is.null(curr$code_run)) {
          out <- c(out, "```{r, eval=ecodown::examples_run()}", curr$code_run, "```")
        } 
        if(!is.null(curr$code_dont_run)) {
          out <- c(out, "```{r, eval=ecodown::examples_not_run()}", curr$code_dont_run, "```")
        } 
        
      }
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
      if(length(out) > 1) out <- reduce(out, function(x, y) c(x, "", y), .init = NULL)
    }

    out <- list(out)  
    names(out) <- curr_name
    
    res <- c(res, out)
  }
  
  res  
  
}

reference_arguments <- function(x) {
  lines <- map_chr(x, ~ paste0(.x[[1]], " | ", paste0(.x[[2]], collapse = "<br>")))
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

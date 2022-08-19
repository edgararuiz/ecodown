reference_to_list_page <- function(file_in, pkg) {
  file_in %>%
    reference_get_tags(pkg) %>%
    reference_process_tags()
}

reference_get_tags <- function(file_in, pkg) {
  pkg_topics <- pkg$topics
  topic_row <- pkg_topics[pkg_topics$file_in == file_in, ]
  topic <- transpose(topic_row)
  topic_rd <- topic[[1]]$rd
  tag_names <- map_chr(topic_rd, ~ class(.)[[1]])
  tag_split <- split(topic_rd, tag_names)
  tag_split <- tag_split[names(tag_split) != "COMMENT"]
  tag_split <- tag_split[names(tag_split) != "TEXT"]
  tag_split
}

reference_process_tags <- function(x) {
  out <- map(x, ~ {
    x <- .x[[1]]
    class_x <- class(x)[[1]]
    out <- NULL
    if(class_x == "tag_examples") out <- tag_examples(x)
    if(class_x == "tag_usage") out <- tag_usage(x)
    if(class_x == "tag_arguments") out <- tag_arguments(x)
    if(class_x == "tag_section") out <- tag_sections(.x)
    if(is.null(out)) out <- tag_paragraphs(x)
    out
  })
  new_names <- substr(names(x), 5, nchar(names(x)))
  names(out) <- new_names
  out
}

tag_method <- function(x) {
  c(
    paste0("## S3 method for class '", x[[2]],"'"),
    as.character(x[[1]])
  )
  
}

tag_paragraphs <- function(x) {
  rf <- tag_flatten(x)
  
  rf_cr <- NULL
  cr <- NULL
  for(i in seq_along(rf)) {
    cl <- rf[[i]]
    if(cl == new_paragraph_symbol) {
      if(!is.null(cr)) rf_cr <- c(rf_cr, cr)
      cr <- NULL
    } else {
      cr <- paste0(cr, cl, collapse = "")
    }
  }
  rf_cr
}

tag_sections <- function(x) {
  map(x, tag_section)
}

tag_section <- function(x) {
  list(
    title = tag_paragraphs(x[[1]]),
    contents = tag_paragraphs(x[[2]])
  )
}

tag_examples <- function(x) {
  rf <- tag_flatten(x)
  on_examples <- TRUE
  examples_run <- NULL
  examples_dont_run <- NULL
  cr <- NULL
  for(i in seq_along(rf)) {
    cl <- rf[[i]]
    if(cl == new_paragraph_symbol | cl == do_not_run_symbol) {
      if(!is.null(cr)){
        if(on_examples) {
          examples_run <- c(examples_run, cr)  
        } else {
          examples_dont_run <- c(examples_dont_run, cr)  
        }
      }  
      cr <- NULL
    } else {
      cr <- c(cr, cl)
    }
    if(cl == do_not_run_symbol) {
      on_examples <- FALSE
    }
  }
  list(
    code_run = examples_run,
    code_dont_run = examples_dont_run
  )
}

tag_usage <- function(x) {
  out <- tag_examples(x)
  out$code_run
}

new_paragraph_symbol <- "<<<<<<<<<<<<<<<<<<<<<<<<<"
do_not_run_symbol <- ";;;;;;;;;;;;;;;;;;;;;;;;;"

remove_return <- function(x) {
  if(x == "\n") x <- new_paragraph_symbol
  remove_generic(x)
}

remove_return_code <- function(x) {
  if(x == "\n") x <- "```r"
  remove_generic(x)
}

remove_generic <- function(x) {
  if(substr(x, nchar(x), nchar(x)) == "\n") {
    x <- substr(x, 1, nchar(x) - 1)
    x <- paste0(x, " ")
  }
  x
}

tag_single_base <- function(x) {
  res <- NULL
  x_class <- class(x)[[1]]
  if(x_class == "TEXT") res <- x
  if(x_class == "RCODE") res <- x
  if(x_class == "VERB") res <- x
  if(x_class == "tag_method") res <- tag_method(x)
  if(x_class == "tag_href") res <- tag_href(x)
  if(x_class == "tag_code") res <- tag_code(x)
  if(x_class == "tag_verb") res <- tag_code(x)
  if(x_class == "tag_pkg") res <- tag_code(x)
  if(x_class == "tag_usage") res <- tag_code(x)
  if(x_class == "tag_url") res <- tag_url(x)
  if(x_class == "tag_if") res <- ""
  if(x_class == "tag_cr") res <- ""
  if(x_class == "tag_R") res <- "`R`"
  if(x_class == "tag_link") res <- as.character(x[[1]])
  if(x_class == "tag_preformatted") res <- tag_preformatted(x)
  if(x_class == "tag_emph") res <- paste0("**", x, "**")
  if(x_class == "tag_strong") res <- paste0("**", x, "**")
  if(x_class == "tag_cite") res <- paste0("*", x, "*")
  if(x_class == "tag_email") res <- tag_url(x)
  if(x_class == "tag_itemize") res <- tag_itemize1(x)
  if(class(x)[[1]] == "tag_tabular") res <- ""
  res
}

tag_single <- function(x) {
  res <- tag_single_base(x)
  x_class <- class(x)[[1]]
  if(x_class == "tag_dontrun") res <- tag_dontrun(x)
  if(x_class == "tag_enumerate") res <- tag_itemize1(x)
  if(x_class == "tag_describe") res <- tag_describe(x)
  if(x_class == "tag_subsection") res <- tag_sub_section(x)
  if(x_class == "LIST") res <- tag_LIST(x)
  if(is.null(res)) stop(paste0("Class '", class(x)[[1]], "' not recognized. Value: ", x))
  res <- map_chr(res, remove_return)
}

tag_LIST <- function(x) {
  x %>% 
    map(tag_single) %>% 
    paste(collapse = "") %>% 
    paste0("\n")
}

tag_describe <- function(x) {
  out <- x %>%  
    flatten() %>% 
    map(~.x[[1]]) %>% 
    map(tag_single_base)
  out_nulls <- !map_lgl(out, is.null)
  out <- out[out_nulls]
  
  out <- reduce(out, function(x, y) c(x, new_paragraph_symbol, y))
  
  out
}

tag_single_item <- function(x) {
  res <- tag_single_base(x)
  if(class(x)[[1]] == "tag_item") res <- list(new_paragraph_symbol, "-")
  if(is.null(res)) stop(paste0("Class '", class(x)[[1]], "' not recognized"))
  res
}

tag_flatten <-  function(x) {
  x %>%
    map(tag_single) %>% 
    flatten() %>% 
    c(., new_paragraph_symbol)  
}

## -------------------------- RD section tag functions -------------------------- 

tag_arguments <- function(x) {
  res <- list()
  for (i in seq_along(x)) {
    item <- x[[i]]
    if ("tag_item" %in% class(item)) {
      res <- c(
        res, 
        list(list(
          argument = tag_paragraphs(item[[1]]), 
          description = tag_paragraphs(item[[2]])
          )
        ))
    }
  }
  res
}

## -------------------------- Atomic RD tag functions ---------------------------

tag_dontrun <- function(x) {
  out <- x %>% 
    map(tag_single) %>% 
    flatten()
  
  c(do_not_run_symbol, out)
}


tag_sub_section <- function(x) {
  x_class <- map_chr(x, class)
  if(any(x_class == "tag")) {
    out <- map(x, map, tag_single)
  } else {
    out <-  map(x, tag_single)   
  }

  out %>% 
    flatten() %>% 
    map(remove_return) %>% 
    c(., new_paragraph_symbol)
}

tag_itemize1 <- function(x) {
  x %>% 
    map(tag_single_item) %>% 
    flatten() %>% 
    map(remove_return) %>% 
    c(., new_paragraph_symbol)
}

tag_code <- function(x) {
  x %>% 
    map(tag_single_base) %>% 
    reduce(paste0) %>% 
    paste0("`", ., "`")
}

tag_preformatted <- function(x) {
  as.character(x) %>%  
    reduce(function(x, y) c(x, new_paragraph_symbol, y)) %>% 
    c("```", new_paragraph_symbol, ., new_paragraph_symbol, "```")
}

tag_url <- function(x) {
  label_list <- map_chr(x, ~.x)
  label <- paste0(label_list, collapse = " ")
  res <- paste0("[", label, "](", label, ")")
}

tag_href <- function(x) {
  address <- as.character(x[[1]])
  label_list <- map_chr(x[[2]], tag_single_base)
  label <- paste0(label_list, collapse = " ")
  res <- paste0("[", label, "](", address, ")")
}

# FOR PARSING ARGUMENTS INTO QMD LATER
# if(length(desc) > 1) {
#   desc_br <- desc %>% 
#     reduce(function(x, y) paste0(x, " <br> ", y, collapse = ""))
# } else {
#   desc_br <- gsub("\n", " <br> ", desc)
# }


check_rd_parsing <- function(pkg) {
  if(is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- pkg$topics
  files_in <- topics$file_in
  for(i in seq_len(length(files_in))) {
    print(paste(i, " - Processing:", files_in[[i]]))
    tags <-reference_get_tags(files_in[[i]], pkg) 
    reference_process_tags(tags)
  }
}

check_dont_runs <- function(pkg) {
  if(is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- pkg$topics
  files_in <- topics$file_in
  out <- NULL
  for(i in seq_len(length(files_in))) {
    print(paste(i, " - Processing:", files_in[[i]]))
    tags <-reference_get_tags(files_in[[i]], pkg) 
    x <- reference_process_tags(tags)
    code_run <- x$examples$code_run
    if(!is.null(code_run)) {
      
    }
  }
}




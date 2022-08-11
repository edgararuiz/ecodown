reference_index <- function(pkg = NULL, quarto_sub_folder, version_folder,
                            reference_folder, vignettes_folder, output) {
  
  ref_list <- reference_to_list_index(pkg)
  
  dir_out <- path(quarto_sub_folder, version_folder, reference_folder)
  
  ref_convert <- reference_index_convert(ref_list, dir_out)
  
  res <- purrr::imap(ref_convert, ~ c(" ", paste("##", .y), " ", .x))
  
  res <- reduce(res, c)
  
  if(output == "qmd") {
    res <- c("---", " ", "---", res)
  }
  
  res
}

reference_index_convert <- function(index_list, dir_out = "") {
  out <- map(index_list, ~ map(.x, ~ {
    
    # Manual fixes of special characters in funs variable
    funcs <- gsub("&lt;", "<", .x$funs)
    funcs <- gsub("&gt;", ">", funcs)
    funcs <- paste0(funcs, collapse = " ")
    
    file_out <- path(dir_out, .x$file_out)
    desc <- .x$title
    c(
      paste0("[", funcs, "](", file_out,")"),
      desc
    )
  }))
  header <- c("Function(s) | Description", "|---|---|")
  map(out, ~ c(header, map_chr(.x, ~ paste0("|", .x[[1]], "|", .x[[2]], "|")) ))
}

reference_to_list_index <- function(pkg) {
  pkg_ref <- pkg$meta$reference
  
  pkg_topics <- pkg$topics
  topics_env <- match_env(pkg_topics)
  
  if (is.null(pkg_ref)) pkg_ref <- list(data.frame(contents = pkg_topics$name))
  
  sections_list <- map(
    seq_along(pkg_ref),
    ~ {
      ref <- pkg_ref[[.x]]
      topic_list <- map(
        ref$contents,
        ~ {
          item_numbers <- NULL
          try(
            item_numbers <- eval(parse(text = paste0("`", .x,"`")), topics_env), 
            silent = TRUE
            )
          if(is.null(item_numbers)) {
            item_numbers <- eval(parse(text = .x), topics_env)
          }
          item_numbers
        }
      )
      topic_ids <- as.numeric(flatten(topic_list))
      transpose(pkg_topics[topic_ids, ])
    })
  
  sections_title <- map_chr(pkg_ref, ~.x$title)
  
  names(sections_list) <- sections_title
  
  sections_list
}
md_reference_index <- function(pkg = NULL, quarto_sub_folder, version_folder,
                            reference_folder, vignettes_folder) {
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
          func <- .x
          if(is_infix(func)) func <- paste0("`", func, "`")
          eval(parse_expr(func), topics_env)
        }
      )
      topic_ids <- as.numeric(flatten(topic_list))
      
      refs_html <- map(
        topic_ids, 
        ~ {
          me <- pkg_topics[.x, ]
          fns <- me$funs[[1]]
          if (length(fns) > 0) {
            alias <- me$alias[[1]]
            alias_func <- paste0(alias, "()")
            title <- gsub("\n", " ", me$title)
            n_path <- path("/", quarto_sub_folder, version_folder, reference_folder, me$file_out)
            fn2 <- paste0("[", fns, "](", n_path, ")")
            fn3 <- paste0(fn2, collapse = " ")
            fn3 <- paste0(fn3, " | ", title)
          }
        })
      
      null_refs <- map_lgl(refs_html, is.null)
      
      refs_chr <- refs_html[!null_refs]
      
      ref_section <- c(
        ifelse(!is.null(ref$title), paste0("## ", ref$title), ""),
        "",
        paste0("Function(s) | Description"),
        paste0("------------- |----------------"),
        refs_chr,
        ""
      )
    }
  )
  
  map_chr(flatten(sections_list), ~.x)
}

md_reference_parse_topic <- function(topic, pkg, examples = TRUE) {
  tag_names <- map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)
  if (examples) {
    ref_tag <- md_reference_parse_examples(tags$tag_examples, pkg, "## Examples")
  } else {
    ref_tag <- md_reference_parse_section(tags$tag_examples, "## Examples")
  }
  c(
    paste0("# ", topic$name),
    md_reference_parse_section(tags$tag_title),
    md_reference_parse_section(tags$tag_description, "## Description"),
    md_reference_parse_section(tags$tag_usage, "## Usage"),
    md_reference_parse_section_arguments(tags$tag_arguments, "## Arguments"),
    md_reference_parse_section(tags$tag_details, "## Details"),
    md_reference_parse_section(tags$tag_value, "## Value"),
    ref_tag,
    md_reference_parse_section(tags$tag_seealso, "## See Also")
  )
}

md_reference_parse_section <- function(x, title = NULL) {
  if (!is.null(x)) {
    c(
      "\n",
      title,
      reference_parse_tag(x[[1]]),
      "\n"
    )
  } else {
    ""
  }
}

md_reference_parse_examples <- function(x, pkg, title = NULL) {
  if (!is.null(x)) {
    load_lib <- paste0("library(", pkg$package, ")\n")
    all_code <- c(load_lib, as.character(x[[1]]))
    c("\n", title, code_run(all_code), "\n")
  } else {
    ""
  }
}

md_reference_parse_section_arguments <- function(x, title = NULL) {
  if (!is.null(x)) {
    args_p <- map_chr(x[[1]], ~ {
      et <- .x
      ec <- paste0(map(et, reference_parse_tag), collapse = " | ")
    })
    args_ret <- map_lgl(args_p, ~ .x != "\n")
    args_filter <- args_p[args_ret]
    args_all <- map_chr(args_filter, ~ paste0(strsplit(.x, "\n")[[1]], collapse = " "))
    c(
      "\n",
      title,
      "\n",
      "Argument      |Description",
      "------------- |----------------",
      args_all,
      "\n"
    )
  } else {
    NULL
  }
}

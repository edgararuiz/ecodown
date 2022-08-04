reference_index <- function(pkg = NULL, quarto_sub_folder, version_folder,
                            reference_folder, vignettes_folder, output) {
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

  res <- map_chr(flatten(sections_list), ~.x)
  
  if(output == "qmd") {
    res <- c("---", "---", res)
  }
  
  res
}

reference_parse_topic <- function(topic, pkg, examples, 
                                  output, output_options = list()
                                  ) {
  tag_names <- map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)
  if (examples) {
    ref_tag <- reference_parse_examples(tags$tag_examples, pkg, "## Examples")
  } else {
    ref_tag <- reference_parse_section(tags$tag_examples, "## Examples")
  }
  res <- c(
    paste0("# ", topic$name),
    reference_parse_section(tags$tag_title),
    reference_parse_section(tags$tag_description, "## Description"),
    reference_parse_section(tags$tag_usage, "## Usage"),
    reference_parse_section_arguments(tags$tag_arguments, "## Arguments"),
    reference_parse_section(tags$tag_details, "## Details"),
    reference_parse_section(tags$tag_value, "## Value"),
    ref_tag,
    reference_parse_section(tags$tag_seealso, "## See Also")
  )
  if(output == "qmd") {
    res <- c("---", output_options, "", "---", res)
  }
  res
}

reference_parse_section <- function(x, title = NULL) {
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

reference_parse_examples <- function(x, pkg, title = NULL) {
  if (!is.null(x)) {
    load_lib <- paste0("library(", pkg$package, ")\n")
    all_code <- c(load_lib, as.character(x[[1]]))
    c("\n", title, code_run(all_code), "\n")
  } else {
    ""
  }
}

reference_parse_section_arguments <- function(x, title = NULL) {
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

reference_parse_tag <- function(x) {
  tg_res <- map(x, ~ {
    lv1 <- .x
    if (length(lv1) > 0) {
      if (length(lv1) == 1) {
        res <- reference_parse_line_tag(lv1)
      } else {
        lv2 <- map(lv1, reference_parse_line_tag)
        res <- paste0(lv2, collapse = "")
      }
      if ("tag_code" %in% class(lv1)) res <- paste0("`", res, "`")
      if ("tag_dontrun" %in% class(lv1)) res <- paste0("```r\n", res, "\n```")
    } else {
      res <- ""
    }
    res
  })
  if (all(map_lgl(x, ~ "RCODE" %in% class(.x)))) {
    tg_res <- c("```r", tg_res, "```")
  }
  paste0(tg_res, collapse = "")
}

reference_parse_line_tag <- function(x) {
  tg_res <- map(x, ~ {
    if (length(.x) > 0) {
      res <- as.character(.x)
      if ("RCODE" %in% class(.x)) res <- paste0("`", res, "`")
    } else {
      res <- ""
    }
    res
  })
  if ("tag_item" %in% class(x)) tg_res <- "\n* "
  paste0(tg_res, collapse = "")
}

code_run <- function(x) {
  res <- NULL
  for (i in seq_along(x)) {
    cl <- x[i]
    cls <- cl
    cr <- substr(cl, nchar(cl) - 1, nchar(cl)) == "\n"
    if (cr) cl <- substr(cl, 1, nchar(cl) - 2)
    res <- c(res, cl)
    if (cl != "") {
      if (substr(cl, 1, 1) != "#") {
        out <- capture.output(eval(parse_expr(cl)))
        out1 <- paste0("#> ", out)
        out2 <- paste0(out1, collapse = "\n")
        if (length(out) > 0) res <- c(res, paste0(out2, "\n"))
      }
    }
  }
  p_res <- paste0(res, collapse = "")
  paste0("```r\n", p_res, "\n```")
}

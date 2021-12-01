#' Processes reference/help files
#' @inheritParams package_build_documentation
#' @param reference_folder Sub folder in `project_folder` where the output files
#' will land.
#' @param pkg A `pkgdown` object. If one is passed, then the `pkg_folder` argument
#' will be ignored
#' @export
package_reference_pages <- function(pkg_folder = "",
                                    reference_folder = "reference",
                                    project_folder = "",
                                    root_folder = here::here(),
                                    pkg = NULL) {
  if (is.null(pkg)) pkg <- pkgdown::as_pkgdown(pkg_folder)

  topics <- transpose(pkg$topics)

  walk(
    topics, ~ {
      new_name <- path(path_ext_remove(path_file(.x$file_in)), ext = "md")
      f_name <- path(project_folder, reference_folder, new_name)
      out <- parse_topic(.x)
      writeLines(out, path(root_folder, f_name))
      msg_green("Created: ", f_name)
    }
  )
}

parse_topic <- function(topic) {
  tag_names <- map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)
  c(
    paste0("# ", topic$name),
    parse_section(tags$tag_title),
    parse_section(tags$tag_description, "## Description"),
    parse_section(tags$tag_usage, "## Usage"),
    parse_section_arguments(tags$tag_arguments, "## Arguments"),
    parse_section(tags$tag_details, "## Details"),
    parse_section(tags$tag_value, "## Value"),
    parse_section(tags$tag_examples, "## Examples"),
    parse_section(tags$tag_seealso, "## See Also")
  )
}

parse_section <- function(x, title = NULL) {
  if (!is.null(x)) {
    c(
      "\n",
      title,
      parse_tag(x[[1]]),
      "\n"
    )
  } else {
    ""
  }
}

parse_section_arguments <- function(x, title = NULL) {
  if (!is.null(x)) {
    args_p <- map_chr(x[[1]], ~ {
      et <- .x
      paste0(map(et, parse_tag), collapse = " | ")
    })
    args_ret <- map_lgl(args_p, ~ .x != "\n")
    c(
      "\n",
      title,
      "\n",
      "Argument      |Description",
      "------------- |----------------",
      args_p[args_ret],
      "\n"
    )
  } else {
    NULL
  }
}

parse_tag <- function(x) {
  tg_res <- map(x, ~ {
    lv1 <- .x
    if (length(lv1) > 0) {
      if (length(lv1) == 1) {
        res <- parse_line_tag(lv1)
      } else {
        lv2 <- map(lv1, parse_line_tag)
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

parse_line_tag <- function(x) {
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

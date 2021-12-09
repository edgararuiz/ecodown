#' Processes reference/help files
#' @inheritParams package_build_documentation
#' @param reference_folder Sub folder in `project_folder` where the output files
#' will land.
#' @param pkg A `pkgdown` object. If one is passed, then the `pkg_folder` argument
#' will be ignored
#' @param downlit_options Flag that indicates if the package name should be
#' added to the 'options()' that tells 'downlit' that this is an internal
#' package
#' @param url_prefix String to prefix to the 'downlit' URL's
#' @export
package_reference <- function(pkg_folder = "",
                              root_folder = here::here(),
                              project_folder = "",
                              reference_folder = "reference",
                              downlit_options = TRUE,
                              url_prefix = "") {
  pkg <- pkgdown::as_pkgdown(pkg_folder)

  msg_color_bold("- - - - - - Reference files - - - - - - -", color = blue)

  create_folder_if_missing(path(root_folder, project_folder, reference_folder))

  package_reference_index(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )

  package_reference_pages(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )

  if (downlit_options) {
    downlit_options(
      package = pkg$package,
      url = project_folder,
      url_prefix = url_prefix
    )
  }
}

#' @rdname package_reference
#' @export
package_reference_index <- function(pkg_folder = "",
                                    reference_folder = "reference",
                                    project_folder = "",
                                    root_folder = here::here(),
                                    pkg = NULL,
                                    downlit_options = TRUE,
                                    url_prefix = "") {
  if (is.null(pkg)) pkg <- pkgdown::as_pkgdown(pkg_folder)
  pkg_ref <- pkg$meta$reference
  pkg_topics <- pkg$topics

  # For packages that do not have a _pkgdown.yml spec
  if (is.null(pkg_ref)) pkg_ref <- list(data.frame(contents = pkg_topics$name))

  sections_list <- map(
    pkg_ref, ~ {
      ref <- .x
      matched_names <- map_chr(
        ref$contents,
        ~ {
          cr <- .x
          ma <- map_lgl(pkg_topics$alias, ~ any(cr == .x))
          pkg_topics$name[ma]
        }
      )
      unique_names <- unique(matched_names)

      refs_html <- map(unique_names, ~ {
        me <- pkg_topics[pkg_topics$name == .x, ]
        fns <- me$funs[[1]]
        if (length(fns) > 0) {
          n_path <- path("/", project_folder, reference_folder, "/", me$file_out)
          fn2 <- paste0("[", fns, "](", n_path, ")")
          fn3 <- paste0(fn2, collapse = " ")
          fn3 <- paste0(fn3, " | ", me$title)
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

  sections_chr <- map_chr(flatten(sections_list), ~.x)

  index_file <- path(project_folder, reference_folder, "index.md")

  writeLines(
    sections_chr,
    path(root_folder, index_file)
  )
  msg_color("Created: ", index_file, color = green)

  if (downlit_options) {
    downlit_options(
      package = pkg$package,
      url = project_folder,
      url_prefix = url_prefix
    )
  }
}

#' @rdname package_reference
#' @export
package_reference_pages <- function(pkg_folder = "",
                                    reference_folder = "reference",
                                    project_folder = "",
                                    root_folder = here::here(),
                                    pkg = NULL,
                                    downlit_options = TRUE,
                                    url_prefix = "") {
  if (is.null(pkg)) pkg <- pkgdown::as_pkgdown(pkg_folder)

  topics <- transpose(pkg$topics)

  walk(
    topics, ~ {
      new_name <- path(path_ext_remove(path_file(.x$file_in)), ext = "md")
      f_name <- path(project_folder, reference_folder, new_name)
      out <- parse_topic(.x)
      writeLines(out, path(root_folder, f_name))
      msg_color("Created: ", f_name, color = green)
    }
  )

  if (downlit_options) {
    downlit_options(
      package = pkg$package,
      url = project_folder,
      url_prefix = url_prefix
    )
  }
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

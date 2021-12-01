#' @rdname package_reference_pages
#' @export
package_reference_index <- function(pkg_folder = "",
                                    reference_folder = "reference",
                                    project_folder = "",
                                    root_folder = here::here(),
                                    pkg = NULL) {
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
  msg_green("Created: ", index_file)
}

diff_files <- function(quarto_folder) {
  e_file <- path(quarto_folder, ".ecodown")

  if (!file_exists(e_file)) {
    msg_color("No '.ecodown' file found", color = red)
    return("full")
  }

  ecodown_file <- readLines(e_file)[[1]]

  msg_color("Getting commits")
  all_commits <- commits()

  all_shas <- map_chr(all_commits, ~ .x$sha)

  y <- all_commits[all_shas == ecodown_file][[1]]
  x <- last_commit()

  x_lab <- paste0("'", substr(x$sha, 1, 7), "...'")
  y_lab <- paste0("'", substr(y$sha, 1, 7), "...'")

  msg_color("Comparing commits:", x_lab, "and", y_lab, color = green)

  all_diffs <- diff(tree(y), tree(x))

  all_files <- map_chr(all_diffs$files, ~ .x$new_file)

  all_ext <- path_ext(all_files)

  full_refresh <- FALSE

  quarto_file <- any(path_file(all_files) == "_quarto.yml")
  if (quarto_file) {
    msg_color("FULL UPDATE TRIGGERED - Quarto file update", color = yellow)
  } else {
    msg_color("Quarto file not updated", color = blue)
  }
  js_ext <- any(all_ext == "js")
  if (js_ext) {
    msg_color("FULL UPDATE TRIGGERED - JavaScript files updated", color = yellow)
  } else {
    msg_color("No JS file changes", color = blue)
  }
  css_ext <- any(all_ext == "css")
  if (css_ext) {
    msg_color("FULL UPDATE TRIGGERED - CSS files updated", color = yellow)
  } else {
    msg_color("No CSS file changes", color = blue)
  }

  full_refresh <- any(c(quarto_file, js_ext, css_ext))

  if (!full_refresh) {
    is_rend <- path_ext(all_files) %in% c("md", "qmd", "Rmd")

    rend_files <- all_files[is_rend]

    rend_abs <- path_abs(rend_files)

    qi <- yaml::read_yaml("_quarto.yml")

    side_bar <- qi$website$sidebar

    list_href <- function(x) {
      ft <- map(x, ~ {
        x_class <- class(.x)
        if (x_class == "list") {
          xc <- .x
          map(xc, ~ {
            xr <- .x$href
            if (is.null(xr)) {
              xt <- .x$contents
              if (is.list(xt)) {
                xr <- map(xt, ~ .x$href)
              }
            }
            xr
          })
        }
      })
      flatten(ft)
    }

    side_bar_parsed <- map(side_bar, ~ {
      x1 <- .x
      x1_href <- x1$href
      x2 <- list_href(x1)
      if (!is.null(x1_href)) {
        x3 <- c(x2, x1_href)
      } else {
        x3 <- x2
      }
      flatten(x3)
    })

    sb_abs <- map(side_bar_parsed, path_abs)

    rend_mtc <- map(
      rend_abs, ~ {
        x1 <- .x
        rf <- map_lgl(sb_abs, ~ any(.x == x1))
        if (any(rf)) {
          reduce(sb_abs[rf], function(x, y) c(x, y))
        } else {
          NULL
        }
      }
    )

    if (length(rend_mtc) > 0) {
      rend_red <- reduce(rend_mtc, function(x, y) c(x, y))
    } else {
      rend_red <- NULL
    }

    files_diff <- unique(c(rend_abs, rend_red))

    msg_color(length(files_diff), "renderable files to be updated", color = blue)

    files_diff
  } else {
    "full"
  }
}

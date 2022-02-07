#' Autolink function calls
#' @inheritParams ecodown_clone_convert
#' @param render_folder Location of the sub-folder that contains the output
#' from Quarto. It defaults to the 'output-dir' entry in the '_quarto.yml' file.
#' @export
ecodown_autolink <- function(quarto_folder = here::here(),
                             render_folder = qe(quarto_folder, "project", "output-dir"),
                             verbosity = c("verbose", "summary", "silent")) {
  verbosity <- verbosity[1]
  ecodown_context_set("verbosity", verbosity)

  downlit_env(quarto_folder = quarto_folder)

  quarto_path <- path(quarto_folder, render_folder)

  html_files <- dir_ls(
    quarto_path,
    recurse = TRUE,
    type = "file",
    glob = "*.html"
  )

  msg_color_title("Auto-linking")

  file_tree(
    file_list = html_files,
    file_type = "html ",
    base_folder = quarto_path,
    command_name = "downlit_single",
    verbosity = verbosity
  )
}

downlit_env <- function(quarto_folder) {
  config_path <- path(quarto_folder, "_ecodown.yml")

  site_url <- qe(quarto_folder, "website", "site-url")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)
    map(
      config_yaml$site$packages, ~ {
        version_folder <- NULL
        versions <- .x$versions
        if (!is.null(versions)) {
          p1 <- keep(versions, ~ !is.null(.x$current))
          p2 <- keep(p1, ~ .x$current)
          if (length(p2) > 0) {
            version_folder <- p2[[1]]$version_folder
          }
        } else {
          version_folder <- .x$version_folder
        }
        pkg_name <- .x$name %||% path_file(.x$repo_url)
        downlit_options(
          package = pkg_name,
          url = path(.x$quarto_sub_folder, version_folder),
          site_url = site_url
        )
      }
    )
  } else {
    if (downlit_null()) stop(
      "No downlit packages loaded in environment, and no '_ecodown.yml' file found on path"
      )
  }
}

downlit_single <- function(input = "") {
  downlit_html_path(input, input)
}

downlit_null <- function() {
  d_attached <- getOption("downlit.attached")
  d_local <- getOption("downlit.local_packages")
  ifelse(is.null(d_attached) && is.null(d_local), TRUE, FALSE)
}

downlit_options <- function(package = "", url = "", site_url = "") {
  if (package != "") {
    d_attached <- getOption("downlit.attached")
    d_local <- getOption("downlit.local_packages")
    new_attached <- unique(c(d_attached, package))
    wo_package <- d_local[names(d_local) != package]
    new_pkg <- setNames(paste(site_url, url, sep = "/"), package)
    new_local <- c(new_pkg, wo_package)
    options("downlit.attached" = new_attached)
    options("downlit.local_packages" = new_local)
  }
}

#' Autolink function calls
#' @inheritParams ecodown_clone_convert
#' @param render_folder Location of the sub-folder that contains the output
#' from Quarto. It defaults to the 'output-dir' entry in the '_quarto.yml' file.
#' @export
ecodown_autolink <- function(quarto_folder = here::here(),
                             render_folder = qe(quarto_folder, "project", "output-dir"),
                             verbosity = c("verbose", "summary", "silent")) {
  
  qbf <- quarto_folder
  
  if(downlit_null()) {
    site_url <- qe(qbf, "site", "site-url")  
    config_path <- path(qbf, "_ecodown.yml")
    if (file_exists(config_path)) {
      config_yaml <- read_yaml(config_path)
      map(
        config_yaml$site$packages, ~{
          if(is.null(.x$name)) {
            pkg_name <- path_file(.x$repo_url)  
          } else {
            pkg_name <- .x$name
          }
          downlit_options(
            package = pkg_name,
            url = pkg_name,
            site_url = site_url
          )
        }
      )
    }
    cat("Packages")
    print(getOption("downlit.local_packages"))
  }
  
  
  verbosity <- verbosity[1]
  
  ecodown_context_set("verbosity", verbosity)

  quarto_path <- path(quarto_folder, render_folder)

  html_files <- dir_ls(quarto_path,
    recurse = TRUE,
    type = "file",
    glob = "*.html"
  )

  msg_color_title("Auto-linking")
  msg_summary_title("Auto-linking")
  
  file_tree(
    file_list = html_files,
    file_type = "html ",
    base_folder = path(quarto_folder, render_folder),
    command_name = "downlit_single",
    verbosity = verbosity
  )
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

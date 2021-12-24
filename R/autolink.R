#' Autolink function calls
#' @inheritParams ecodown_clone_convert
#' @param render_folder Location of the sub-folder that contains the output
#' from Quarto. It defaults to the 'output-dir' entry in the '_quarto.yml' file.
#' @export
ecodown_autolink <- function(quarto_folder = here::here(),
                             render_folder = qe(quarto_folder, "project", "output-dir"),
                             verbosity = c("summary", "verbose", "silent")) {
  ecodown_context_set("verbosity", verbosity)

  quarto_path <- path(quarto_folder, render_folder)

  html_files <- dir_ls(quarto_path,
    recurse = TRUE,
    type = "file",
    glob = "*.html"
  )

  msg_color_title("Auto-linking")
  msg_color(bold("Path: "), quarto_path, color = green)

  msg_summary_title("Autolinking")
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

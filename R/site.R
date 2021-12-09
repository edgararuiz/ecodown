#' Autolink function calls
#' @param quarto_folder Location of the sub-folder that contains the output from
#' Quarto.
#' @export
site_autolink_html <- function(quarto_folder = "_site") {
  msg_color("- - - - - - Auto-linking - - - - - - - - -", color = blue)
  html_files <- dir_ls(quarto_folder, recurse = TRUE, type = "file", glob = "*.html")
  walk(
    html_files,
    ~ {
      downlit_html_path(.x, .x)
      msg_color("Processed: ", path_file(.x), color = green)
    }
  )
}

downlit_options <- function(package = "", url = "", url_prefix = "") {
  if (package != "") {
    d_attached <- getOption("downlit.attached")
    d_local <- getOption("downlit.local_packages")
    new_attached <- unique(c(d_attached, package))
    wo_package <- d_local[names(d_local) != package]
    new_pkg <- setNames(paste0(url_prefix, url, collapse = "/"), package)
    new_local <- c(new_pkg, wo_package)
    options("downlit.attached" = new_attached)
    options("downlit.local_packages" = new_local)
  }
}

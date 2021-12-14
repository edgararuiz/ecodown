#' Autolink function calls
#' @param rendered_site Location of the sub-folder that contains the output
#' from Quarto. It defaults to the 'output-dir' entry in the '_quarto.yml' file.
#' @export
site_autolink_html <- function(rendered_site = get_quarto_entry(here::here(), "project", "output-dir")) {
  msg_color("- - - - - - Auto-linking - - - - - - - - -", color = blue)
  html_files <- dir_ls(rendered_site, recurse = TRUE, type = "file", glob = "*.html")
  walk(
    html_files,
    ~ {
      downlit_html_path(.x, .x)
      msg_color("Processed: ", path_file(.x), color = green)
    }
  )
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

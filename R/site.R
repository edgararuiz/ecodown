#' Autolink function calls 
#' @param quarto_folder Location of the sub-folder that contains the output from
#' Quarto. 
#' @export
site_autolink_html <- function(quarto_folder = "_site") {
  html_files <- dir_ls(quarto_folder, recurse = TRUE, type = "file", glob = "*.html")
  walk(
    html_files,
    ~ {
      downlit_html_path(.x, .x)
      msg_green("Processed: ", path_file(.x))
    }
  )
} 



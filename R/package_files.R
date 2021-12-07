#' Copies individual files from package to Quarto
#'
#' @inheritParams package_build_documentation
#' @param target Sub folder in `project_folder` where the output file will land
#' @param file_names A vector of one or many file names. If it is multiple files
#' the function will look for and use the first available.
#' @param override_name File name of the output, in case the original file name
#' needs to be changed.
#' @export
package_readme <- function(pkg_folder = "",
                           target = "",
                           file_names = c("readme.md"),
                           project_folder = "",
                           root_folder = here::here()) {
  package_file_copy(
    pkg_folder = pkg_folder,
    target = target,
    file_names = file_names,
    override_name = "index.md",
    project_folder = project_folder,
    root_folder = root_folder
  )
}

#' @rdname package_readme
#' @export
package_news <- function(pkg_folder = "",
                         target = "",
                         file_names = c("news.md", "news.Rmd"),
                         project_folder = "",
                         root_folder = here::here()) {
  package_file_copy(
    pkg_folder = pkg_folder,
    target = target,
    file_names = file_names,
    project_folder = project_folder,
    root_folder = root_folder
  )
}

#' @rdname package_readme
#' @export
package_file_copy <- function(pkg_folder = "",
                              target = "project_folder",
                              file_names = c("name.md", "name.Rmd"),
                              override_name = NULL,
                              project_folder = "",
                              root_folder = here::here()) {
  
  file_present <- file_exists(path(pkg_folder, file_names))
  file_numbers <- setNames(file_present, 1:length(file_present))
  file_there <- file_numbers[file_numbers == TRUE]
  
  if(length(file_there) > 0) {
    file_min <- min(as.integer(names(file_there)))
    file_use <- file_present[file_min]
    file_name <- names(file_use)
    
    dest_folder <- path(root_folder, project_folder, target)
    
    create_folder_if_missing(dest_folder)
    
    file_n <- ifelse(is.null(override_name), path_file(file_name), override_name)
    
    file_copy(
      file_name,
      path(dest_folder, file_n),
      overwrite = TRUE
    )
    msg_color("Copied: ", path(project_folder, target, file_n), color = green)    
  } else {
    msg_color(
      "File(s) not found: ", paste0(file_names, collapse = ", "), 
      color = yellow
      )
  }

}

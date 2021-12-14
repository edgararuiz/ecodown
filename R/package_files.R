#' Copies individual files from package to Quarto
#'
#' @inheritParams package_build_documentation
#' @param target Sub folder in `quarto_sub_folder` where the output file will land
#' @param file_names A vector of one or many file names. If it is multiple files
#' the function will look for and use the first available.
#' @param override_name File name of the output, in case the original file name
#' needs to be changed.
#' @export
package_readme <- function(package_source_folder = "",
                           target = "",
                           file_names = c("README.md"),
                           quarto_sub_folder = "",
                           quarto_base_folder = here::here()) {
  package_file_copy(
    package_source_folder = package_source_folder,
    target = target,
    file_names = file_names,
    override_name = "index.md",
    quarto_sub_folder = quarto_sub_folder,
    quarto_base_folder = quarto_base_folder
  )
}

#' @rdname package_readme
#' @export
package_news <- function(package_source_folder = "",
                         target = "",
                         file_names = c("NEWS.md", "NEWS.Rmd"),
                         quarto_sub_folder = "",
                         quarto_base_folder = here::here()) {
  package_file_copy(
    package_source_folder = package_source_folder,
    target = target,
    file_names = file_names,
    quarto_sub_folder = quarto_sub_folder,
    quarto_base_folder = quarto_base_folder
  )
}

#' @rdname package_readme
#' @export
package_file_copy <- function(package_source_folder = "",
                              target = "quarto_sub_folder",
                              file_names = c("name.md", "name.Rmd"),
                              override_name = NULL,
                              quarto_sub_folder = "",
                              quarto_base_folder = here::here()) {
  file_present <- file_exists(path(package_source_folder, file_names))
  file_numbers <- setNames(file_present, 1:length(file_present))
  file_there <- file_numbers[file_numbers == TRUE]

  if (length(file_there) > 0) {
    file_min <- min(as.integer(names(file_there)))
    file_use <- file_present[file_min]
    file_name <- names(file_use)

    dest_folder <- path(quarto_base_folder, quarto_sub_folder, target)

    create_folder_if_missing(dest_folder)

    file_n <- ifelse(is.null(override_name), path_file(file_name), override_name)

    file_copy(
      file_name,
      path(dest_folder, file_n),
      overwrite = TRUE
    )
    msg_color("Copied: ", path(quarto_sub_folder, target, file_n), color = green)
  } else {
    msg_color(
      "File(s) not found: ", paste0(file_names, collapse = ", "),
      color = yellow
    )
  }
}

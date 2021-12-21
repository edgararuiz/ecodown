#' Get entry from the Quarto setup file
#' @param quarto_path Path to the Quarto folder, defaults to project path.
#' @param ... A single or multiple entries with the path to the desired entry.
#' @param file_name The name of the Quarto setup file, '_quarto.yml'
#' @export
get_quarto_entry <- function(quarto_path = here::here(), ..., file_name = "_quarto.yml") {
  if (!file_exists(path(quarto_path, file_name))) {
    return("")
  }
  element <- list(...)
  cont <- read_yaml(path(quarto_path, file_name))
  for (i in seq_along(element)) {
    cont <- pluck(cont, element[[i]])
  }
  if (is.null(cont)) cont <- ""
  cont
}

create_folder_if_missing <- function(x) if (!dir_exists(x)) dir_create(x)

msg_color <- function(..., color = black) cat(color(paste0("- ", ..., "\n")))

msg_color_bold <- function(..., color = black) {
  cat(bold(color(paste0("- ", ..., "\n"))))
}

msg_color_title <- function(..., color = blue) {
  title <- paste0(...)
  n_side <- floor(40 - nchar(title) / 2)
  sides <- paste0(rep("- ", times = n_side / 2), collapse = "")
  cat(bold(color(paste0(sides, title, " ", sides, "\n"))))
}

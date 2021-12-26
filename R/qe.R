#' Get entry from the Quarto setup file
#' @param quarto_path Path to the Quarto folder, defaults to project path.
#' @param ... A single or multiple entries with the path to the desired entry.
#' @param file_name The name of the Quarto setup file, '_quarto.yml'
#' @export
qe <- function(quarto_path = here::here(), ..., file_name = "_quarto.yml") {
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

create_folder_if_missing <- function(x) {
  if (!dir_exists(x)) dir_create(x)
}

msg_color <- function(..., color = black) cat(color(paste0("- ", ..., "\n")))

msg_color_bold <- function(..., color = black) cat(bold(color(paste0("- ", ..., "\n"))))

#' Wrapper on the Quarto Render function
#' @inheritParams ecodown_clone_convert
#' @export
ecodown_quarto_render <- function(quarto_folder = here::here(),
                                  verbosity = c("summary", "verbose", "silent")) {
  ecodown_context_set("verbosity", verbosity)

  msg_color_title("Render Quarto site")
  msg_summary_entry("\n")
  msg_summary_title("Render in Quarto")

  rend_files <- c(
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.md"),
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.Rmd"),
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.qmd")
  )

  file_tree(
    file_list = rend_files,
    file_type = "renderable ",
    base_folder = quarto_folder,
    command_name = "quarto_render",
    entry_value = NULL,
    addl_entries = list(as_job = FALSE, quiet = TRUE),
    verbosity = verbosity
  )
}
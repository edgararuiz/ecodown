#' Wrapper on the Quarto Render function
#' @inheritParams ecodown_clone_convert
#' @export
ecodown_quarto_render <- function(quarto_folder = here::here(),
                                  verbosity = c("verbose", "summary", "silent")
                                  ) {
  verbosity <- verbosity[1]
  
  ecodown_context_set("verbosity", verbosity)

  msg_summary_entry("\n")
  msg_color_title("Render Quarto site")

  rend_files <- c(
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.md"),
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.Rmd"),
    dir_ls(quarto_folder, recurse = TRUE, glob = "*.qmd")
  )

  file_tree(
    file_list = rend_files,
    file_type = "renderable ",
    base_folder = quarto_folder,
    command_name = "render_quarto",
    verbosity = verbosity
  )
}

render_quarto <- function(input) {
  if(!is.null(quarto_path())) {
    if(tolower(path_ext(input) == "qmd")) {
      quarto_render(
        input, 
        as_job = FALSE, 
        quiet = TRUE, 
        pandoc_args = c("--use-freezer")
      )      
    } else {
      quarto_render(
        input, 
        as_job = FALSE, 
        quiet = TRUE
      ) 
    }

  } 
}

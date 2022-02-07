#' Wrapper on the Quarto Render function
#' @param autolink Boolean flag that indicates if 'downlit' should run. Defaults
#' to TRUE.
#' @inheritParams ecodown_clone_convert
#' @export
ecodown_quarto_render <- function(quarto_folder = here::here(),
                                  verbosity = c("verbose", "summary", "silent"),
                                  autolink = TRUE
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
  
  if(autolink) downlit_env(quarto_folder = quarto_folder)

  file_tree(
    file_list = rend_files,
    file_type = "renderable ",
    base_folder = quarto_folder,
    command_name = "render_quarto",
    verbosity = verbosity, 
    addl_entries = list(
      autolink = autolink, 
      quarto_folder = quarto_folder
      )
  )
}

render_quarto <- function(input, autolink, quarto_folder) {
  if(!is.null(quarto_path())) {
    if(tolower(path_ext(input) == "qmd")) {
      p_args <- c("--use-freezer")
    } else {
      p_args <- NULL
    }
    quarto_render(
      input = input, 
      as_job = FALSE, 
      quiet = TRUE, 
      pandoc_args = p_args
    ) 
    if(autolink) {
      co <- path_common(c(quarto_folder, input))
      ne <- path_dir(substr(input, nchar(co) + 1, nchar(input)))
      fi <- path(path_ext_remove(path_file(input)), ext = "html")
      od <- qe(quarto_folder, "project", "output-dir")
      input_html <- path(quarto_folder, od, ne, fi)
      if(!file_exists(input_html)) {
        input_html <- path(path_ext_remove(input), ext = "html")
      } 
      downlit_single(input = input_html)
    }
  }
}

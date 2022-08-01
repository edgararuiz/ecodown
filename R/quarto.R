#' Wrapper on the Quarto Render function
#' @param autolink Boolean flag that indicates if 'downlit' should run. Defaults
#' to TRUE.
#' @param run Flag that indicates the mode to render the site. Defaults to 'smart'
#' which attempts to run only the files that changed.
#' @inheritParams ecodown_clone_convert
#' @export
ecodown_quarto_render <- function(quarto_folder = here::here(),
                                  verbosity = c("verbose", "summary", "silent"),
                                  autolink = FALSE,
                                  run = c("smart", "full", "no")) {
  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  msg_summary_entry("\n")
  msg_color_title("Render Quarto site")

  run <- run[1]
  run <- null_true(run)
  run <- ifelse(run == TRUE, "smart", run)

  rend_files <- NULL
  if (run == "smart") rend_files <- diff_files(quarto_folder = quarto_folder)
  if (!is.null(rend_files) && length(rend_files) == 1) {
    if (rend_files == "full") run <- "full"
  }

  if (run == "smart" && !is.null(rend_files)) {
    ident_readme <- tolower(path_file(rend_files)) == "readme.md"

    rend_files <- rend_files[!ident_readme]

    if (length(rend_files) > 0) {
      if (autolink) downlit_env(quarto_folder = quarto_folder)

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
    } else {
      msg_color("No pages to update", color = green)
      msg_summary_entry("[ No pages to update ]\n\n")
    }
  }

  if (run == "full") {
    quiet_flag <- ifelse(verbosity == "silent", TRUE, FALSE)

    quarto_render(
      input = quarto_folder,
      as_job = FALSE,
      quiet = quiet_flag,
      pandoc_args = "--use-freezer"
    )
  }
}

render_quarto <- function(input, autolink, quarto_folder) {
  if (!is.null(quarto_path()) && file_exists(input)) {
    p_args <- NULL
    if (tolower(path_ext(input) == "qmd")) {
      fm <- read_front_matter(input)
      fm_freeze <- fm$execute$freeze
      if (!is.null(fm_freeze)) {
        if (fm_freeze) p_args <- c("--use-freezer")
      }
    }
    quarto_render(
      input = input,
      as_job = FALSE,
      quiet = TRUE,
      pandoc_args = p_args
    )
    if (autolink) {
      co <- path_common(c(quarto_folder, input))
      ne <- path_dir(substr(input, nchar(co) + 1, nchar(input)))
      fi <- path(path_ext_remove(path_file(input)), ext = "html")
      od <- qe(quarto_folder, "project", "output-dir")
      input_html <- path(quarto_folder, od, ne, fi)
      if (!file_exists(input_html)) {
        input_html <- path(path_ext_remove(input), ext = "html")
      }
      downlit_single(input = input_html)
    }
  }
}

#' Prepare full Quarto site
#' @param quarto_folder Base target Quarto folder. Defaults to current workspace.
#' @param verbosity Level of messaging available during run time. Possible values
#' are 'verbose', 'summary', and 'silent'.
#' @export
ecodown_build <- function(quarto_folder = here::here(),
                          verbosity = c("summary", "verbose", "silent")) {
  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  qbf <- quarto_folder
  config_path <- path(qbf, "_ecodown.yml")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)
    walk(
      config_yaml$site$packages, ~ {
        exec(
          "ecodown_clone_convert",
          !!!.x,
          quarto_folder = qbf,
          verbosity = verbosity
        )
      }
    )

    msg_summary_entry("\n")
    exec_command(
      "ecodown_quarto_render",
      config_yaml$site$quarto,
      addl_entries = list(quarto_folder = qbf, verbosity = verbosity)
    )

    msg_summary_entry("\n")
    exec_command(
      "ecodown_autolink",
      config_yaml$site$autolink,
      list(quarto_folder = qbf, verbosity = verbosity)
    )

    msg_color_title("Complete")
  }
}

#' Prepare full Quarto site
#' @inheritParams package_clone_and_build
#' @export
site_build_quarto <- function(quarto_base_folder = here::here(),
                              verbosity = c("summary", "verbose", "silent")
                              ) {

  ecodown_context_set("verbosity", verbosity)
  
  qbf <- quarto_base_folder
  config_path <- path(qbf, "_ecodown.yml")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)

    msg_summary_title("Package clone and prep")
    msg_summary_entry("       Clone / Checkout       | R N Art Ref |\n")
    msg_summary_entry("------------------------------|-------------|\n")
     
    walk(
      config_yaml$site$packages, ~ {
        exec(
          "package_clone_and_build",
          !!!.x,
          quarto_base_folder = qbf
        )
      }
    )

    msg_summary_entry("\n")
    exec_command(
      "site_quarto_build",
      addl_entries = list(quarto_base_folder = qbf, verbosity = verbosity)
    )
        
    msg_summary_entry("\n")
    exec_command(
      "site_autolink_html",
      config_yaml$site$autolink,
      list(quarto_base_folder = qbf, verbosity = verbosity)
      )

    msg_color_title("Complete")
  }
}

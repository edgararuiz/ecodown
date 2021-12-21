#' Prepare full Quarto site
#' @inheritParams package_clone_and_build
#' @export
site_build_quarto <- function(quarto_base_folder = here::here()) {
  qbf <- quarto_base_folder
  config_path <- path(qbf, "_ecodown.yml")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)

    walk(
      config_yaml$site$packages, ~ {
        exec(
          "package_clone_and_build",
          !!!.x,
          quarto_base_folder = qbf
        )
      }
    )
    exec_command("quarto_render", config_yaml$site$quarto_render, qbf)
    exec_command("site_autolink_html", config_yaml$site$autolink, qbf)

  }
}

exec_command <- function(command_name = "", 
                        entry_value, 
                        quarto_base_folder = ""
                        ) {
  if(!is.null(entry_value)) {
    if(entry_value != FALSE) {
      exec(command_name, entry_value)
    } 
  } else {
    exec(command_name, quarto_base_folder = quarto_base_folder)
  }
  
  
}






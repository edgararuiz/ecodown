#' Prepare full Quarto site
#' @inheritParams package_clone_and_build
#' @export
site_build_quarto <- function(quarto_base_folder = here::here(),
                              verbosity = c("verbose", "summary", "silent")
                              ) {

  ecodown_context_set("verbosity", verbosity)
  
  qbf <- quarto_base_folder
  config_path <- path(qbf, "_ecodown.yml")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)

    msg_summary_entry(">> Package clone and prep:\n")
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
    msg_color_title("Render Quarto site")
    
    run_quiet <- get_verbosity() != "verbose"
    
    msg_summary_entry(">> Render in Quarto\n")
    
    msg_summary_tree(
      c(
        dir_ls(quarto_base_folder, recurse = TRUE, glob = "*.md"),
        dir_ls(quarto_base_folder, recurse = TRUE, glob = "*.Rmd"),
        dir_ls(quarto_base_folder, recurse = TRUE, glob = "*.qmd")
      ),
      "renderable ",
      base_folder = quarto_base_folder
    )
    
    msg_summary_entry("------ Process started\n")
    exec_command(
      "quarto_render", 
      config_yaml$site$quarto_render, 
      list(input = qbf, as_job = FALSE, quiet = run_quiet)
      )
    msg_summary_entry("------ Process complete\n")
    
    exec_command(
      "site_autolink_html", 
      config_yaml$site$autolink, 
      list(quarto_base_folder = qbf)
      )

    msg_color_title("Complete")
  }
}

exec_command <- function(command_name = "", 
                        entry_value, 
                        ...
                        ) {
  x <- FALSE
  if(!is.null(entry_value)) {
    if(entry_value != FALSE) {
      x <- TRUE
    }
  } else {
    x <- TRUE
  }
  
  if(x) {
    entries <- c(entry_value, ...)
    exec(command_name, !!! entries)  
  }
  
}






#' @export
examples_run <- function() {
  examples_run_not_run("reference_examples_not_run")
}

#' @export
examples_not_run <- function() {
  examples_run_not_run("reference_examples")
}

examples_run_not_run <- function(x = "") {
  out <- FALSE
  env_var <- Sys.getenv(paste0("ecodown_", x), unset = NULL)
  if(is.null(env_var)) {
    yml_path <- path("_ecodown.yml")
    if(file_exists(yml_path)) {
      yml_file <- read_yaml(yml_path)  
      re <- yml_file$site$clone_convert
      if (!is.null(re[[x]]))  out <- re[[x]]
    }  
  } else {
    out <- env_var
  }
  
  out
}



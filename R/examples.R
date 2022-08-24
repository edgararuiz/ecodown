#' Functions that flag if the example is to run
#' @details These functions are placed in the converted `.qmd` files
#' by `ecodown_convert()`. They serve as flexible flags that indicate if the
#' examples, and examples don't run are executed when rendering Quarto.
#' @export
examples_run <- function() {
  examples_run_not_run("reference_examples")
}

#' @rdname examples_run
#' @export
examples_not_run <- function() {
  examples_run_not_run("reference_examples_not_run")
}

examples_run_not_run <- function(x = "") {
  out <- FALSE
  env_var <- Sys.getenv(paste0("ecodown_", x), unset = NA)
  if(is.na(env_var)) {
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



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
  yml_path <- path("_ecodown.yml")
  if(file_exists(yml_path)) {
    yml_file <- read_yaml(yml_path)  
    re <- yml_file$site$clone_convert
    out <- re[[x]]
  }
  out
}

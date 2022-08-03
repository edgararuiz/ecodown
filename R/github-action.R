#' Copies a GHA file that automates the site re-build
#' @details Copies a GHA file called `ecodown.yaml` that automates the package
#' cloning, document copying, reference creation and then commits the changes 
#' back to the repo. 
#' @param project_folder The location of the projects root folder
#' @export
ecodown_github_action <- function(project_folder = ".") {
  dest_path <- path(workflow_folder, "ecodown.yaml")
  
  if (file_exists(dest_path)) stop(dest_path, " already exists.")
  
  github_folder <- path(project_folder, ".github")
  workflow_folder <- path(github_folder, "workflows")
  if (!dir_exists(github_folder)) dir_create(github_folder)
  if (!dir_exists(workflow_folder)) dir_create(workflow_folder)
  
  installed_folder <- system.file(path("gha", "ecodown.yaml"), package = "ecodown")
  dev_folder <- path(project_folder, "inst", "gha", "ecodown.yaml")
  
  if(file_exists(dev_folder)) {
    yaml_path <- dev_folder
  } else {
    yaml_path <- installed_folder
  }
  
  file_copy(yaml_path, dest_path)
  cat(bold(blue("i")), "Copying ", blue(dest_path))
}
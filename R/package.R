#' Clones repo and builds documentation
#' @inheritParams package_build_documentation
#' @inheritParams package_clone_git_repo
#' @export
package_clone_and_build <- function(project_folder = "",
                                    root_folder = here::here(),
                                    readme = TRUE,
                                    news = TRUE,
                                    articles = TRUE,
                                    reference = TRUE,
                                    downlit_options = TRUE,
                                    url_prefix = "",
                                    url = "",
                                    commit = c("latest_tag", "latest_commit"),
                                    target_folder = tempdir(),
                                    branch = "main"
                                    ) {
  
  pkg_path <- package_clone_git_repo(
    url = url,
    commit = commit,
    target_folder = target_folder,
    branch = branch
  )
  
  package_build_documentation(
    pkg_folder = pkg_path,
    project_folder = project_folder,
    root_folder = here::here(),
    readme = readme,
    news = news,
    articles = articles,
    reference = reference,
    downlit_options = downlit_options,
    url_prefix = url_prefix
  )
  
}
#' Copies and/or converts files from package source into Quarto
#' @param pkg_folder Path to the package's source code
#' @param root_folder Base target Quarto folder. Defaults to current workspace.
#' @param project_folder Sub folder in `root_folder` that will be the base for
#' the package's documentation.
#' @param readme Flag that indicates if the README file needs to be processed
#' @param news Flag that indicates if the NEWS file needs to be processed
#' @param articles Flag that indicates if the vignette files needs to be processed
#' @param reference Flag that indicates if the help files needs to be processed
#' @param downlit_options Flag that indicates if the package name should be
#' added to the 'options()' that tells 'downlit' that this is an internal
#' package
#' @param url_prefix String to prefix to the 'downlit' URL's
#' @export
package_build_documentation <- function(pkg_folder = "",
                                        project_folder = "",
                                        root_folder = here::here(),
                                        readme = TRUE,
                                        news = TRUE,
                                        articles = TRUE,
                                        reference = TRUE,
                                        downlit_options = TRUE,
                                        url_prefix = "") {
  if (readme | news) {
    msg_color_bold("- - - - - - - Top files - - - - - - - - -", color = blue)
  }

  if (readme) {
    package_readme(
      pkg_folder = pkg_folder,
      project_folder = project_folder,
      root_folder = root_folder
    )
  }

  if (news) {
    package_news(
      pkg_folder = pkg_folder,
      project_folder = project_folder,
      root_folder = root_folder
    )
  }

  if (articles) {
    package_articles(
      pkg_folder = pkg_folder,
      project_folder = project_folder,
      root_folder = root_folder
    )
  }

  if (reference) {
    package_reference(
      pkg_folder = pkg_folder,
      project_folder = project_folder,
      root_folder = root_folder,
      downlit_options = downlit_options,
      url_prefix = url_prefix
    )
  }
}

#' Copies the vignettes into Quarto
#' @inheritParams package_build_documentation
#' @param source Sub-folder location where the vignettes are available
#' @param target Sub-folder location to place the articles in Quarto
#' @export
package_articles <- function(pkg_folder = "",
                             source = "vignettes",
                             target = "articles",
                             project_folder = "",
                             root_folder = here::here()) {
  msg_color_bold("- - - - - - Article files - - - - - - - -", color = blue)

  a_folder <- path(pkg_folder, source)

  if (dir_exists(a_folder)) {
    dir_copy(
      a_folder,
      path(root_folder, project_folder, target)
    )
    msg_color(
      "Vignette folder copied to: ", path(project_folder, target),
      color = green
    )
  } else {
    msg_color(
      "Vignette folder not found",
      color = yellow
    )
  }
}

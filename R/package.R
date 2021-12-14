#' Clones repo and builds documentation
#' @inheritParams package_build_documentation
#' @inheritParams package_clone_git_repo
#' @export
package_clone_and_build <- function(quarto_sub_folder = "",
                                    quarto_base_folder = here::here(),
                                    readme = TRUE,
                                    news = TRUE,
                                    articles = TRUE,
                                    reference = TRUE,
                                    downlit_options = TRUE,
                                    url_prefix = "",
                                    repo_url= "",
                                    commit = c("latest_tag", "latest_commit"),
                                    target_folder = tempdir(),
                                    branch = "main"
                                    ) {
  
  pkg_path <- package_clone_git_repo(
    repo_url= repo_url,
    commit = commit,
    target_folder = target_folder,
    branch = branch
  )
  
  package_build_documentation(
    package_source_folder = pkg_path,
    quarto_sub_folder = quarto_sub_folder,
    quarto_base_folder = here::here(),
    readme = readme,
    news = news,
    articles = articles,
    reference = reference,
    downlit_options = downlit_options,
    url_prefix = url_prefix
  )
  
}
#' Copies and/or converts files from package source into Quarto
#' @param package_source_folder Path to the package's source code
#' @param quarto_base_folder Base target Quarto folder. Defaults to current workspace.
#' @param quarto_sub_folder Sub folder in `quarto_base_folder` that will be the base for
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
package_build_documentation <- function(package_source_folder = "",
                                        quarto_sub_folder = "",
                                        quarto_base_folder = here::here(),
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
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (news) {
    package_news(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (articles) {
    package_articles(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (reference) {
    package_reference(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder,
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
package_articles <- function(package_source_folder = "",
                             source = "vignettes",
                             target = "articles",
                             quarto_sub_folder = "",
                             quarto_base_folder = here::here()) {
  msg_color_bold("- - - - - - Article files - - - - - - - -", color = blue)

  a_folder <- path(package_source_folder, source)

  if (dir_exists(a_folder)) {
    dir_copy(
      a_folder,
      path(quarto_base_folder, quarto_sub_folder, target)
    )
    msg_color(
      "Vignette folder copied to: ", path(quarto_sub_folder, target),
      color = green
    )
  } else {
    msg_color(
      "Vignette folder not found",
      color = yellow
    )
  }
}

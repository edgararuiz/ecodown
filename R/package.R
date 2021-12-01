#' Copies and/or converts files from package source into Quarto
#' @param pkg_folder Path to the package's source code
#' @param root_folder Base target Quarto folder. Defaults to current workspace.
#' @param project_folder Sub folder in `root_folder` that will be the base for
#' the package's documentation.
#' @param readme Flag that indicates of the README file needs to be processed
#' @param news Flag that indicates of the NEWS file needs to be processed
#' @param articles Flag that indicates of the vignette files needs to be processed
#' @param reference Flag that indicates of the help files needs to be processed
#' @export
package_build_documentation <- function(pkg_folder = "",
                                        project_folder = "",
                                        root_folder = here::here(),
                                        readme = TRUE,
                                        news = TRUE,
                                        articles = TRUE,
                                        reference = TRUE) {
  if (readme | news) msg_bold_blue("- - - - - - - Top files - - - - - - - - -")

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
      root_folder = root_folder
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
  msg_bold_blue("- - - - - - Article files - - - - - - - -")

  a_folder <- path(pkg_folder, source)

  if (dir_exists(a_folder)) {
    full_file_copy(
      a_folder,
      path(root_folder, project_folder, target)
    )
    msg_green("Vignette folder copied to", path(project_folder, target))
  } else {
    msg_yellow("Vignette folder not found")
  }
}

#' @rdname package_reference_pages
#' @export
package_reference <- function(pkg_folder = "",
                              root_folder = here::here(),
                              project_folder = "",
                              reference_folder = "reference") {
  pkg <- pkgdown::as_pkgdown(pkg_folder)

  msg_bold_blue("- - - - - - Reference files - - - - - - -")

  create_folder_if_missing(path(root_folder, project_folder, reference_folder))

  package_reference_index(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )

  package_reference_pages(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )
}

#' Download the package's latest source code from repo
#' @param url Repo location
#' @param target_folder Location to copy the package to. Defaults to a temporary
#' directory
#' @param branch Repo branch. Defaults to 'main'
#' @export
package_clone_git_repo <- function(url = "",
                                   target_folder = tempdir(),
                                   branch = "main") {
  tf <- path(target_folder, path_file(url))
  system(paste0("git clone ", url, " -b ", branch, " ", tf))
  tf
}

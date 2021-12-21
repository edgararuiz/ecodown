#' Clones repo and builds documentation
#' @inheritParams package_build_documentation
#' @inheritParams package_clone_git_repo
#' @param quarto_sub_folder Sub folder in `quarto_base_folder` that will be the
#' base for the package's documentation. It defaults to the last section of the
#' 'repo_url', which is usually the package name.
#' @export
package_clone_and_build <- function(repo_url = "",
                                    quarto_sub_folder = path_file(repo_url),
                                    quarto_base_folder = here::here(),
                                    convert_readme = TRUE,
                                    convert_news = TRUE,
                                    convert_articles = TRUE,
                                    convert_reference = TRUE,
                                    downlit_options = TRUE,
                                    site_url = get_quarto_entry(quarto_base_folder, "site", "site-url"),
                                    commit = c("latest_tag", "latest_commit"),
                                    target_folder = tempdir(),
                                    branch = "main") {
  
  msg_color_title("Pakcage documentation")

  if (quarto_base_folder == here::here()) {
    msg_color(bold("quarto_base_folder: "), here::here(), color = green)
  }

  pkg_name <- path_file(repo_url)
  if (quarto_sub_folder == pkg_name) {
    msg_color(bold("quarto_sub_folder: "), pkg_name, color = green)
  }

  quarto_site <- get_quarto_entry(quarto_base_folder, "site", "site-url")
  if (site_url == quarto_site) {
    msg_color(bold("site_url: "), quarto_site, color = green)
  }

  pkg_path <- package_clone_git_repo(
    repo_url = repo_url,
    commit = commit,
    target_folder = target_folder,
    branch = branch
  )

  package_build_documentation(
    package_source_folder = pkg_path,
    quarto_sub_folder = quarto_sub_folder,
    quarto_base_folder = quarto_base_folder,
    convert_readme = convert_readme,
    convert_news = convert_news,
    convert_articles = convert_articles,
    convert_reference = convert_reference,
    downlit_options = downlit_options,
    site_url = site_url
  )
}
#' Copies and/or converts files from package source into Quarto
#' @param package_source_folder Path to the package's source code
#' @param quarto_base_folder Base target Quarto folder. Defaults to current workspace.
#' @param quarto_sub_folder Sub folder in `quarto_base_folder` that will be the base for
#' the package's documentation.
#' @param convert_readme Flag that indicates if the README file needs to be processed
#' @param convert_news Flag that indicates if the NEWS file needs to be processed
#' @param convert_articles Flag that indicates if the vignette files needs to be processed
#' @param convert_reference Flag that indicates if the help files needs to be processed
#' @param downlit_options Flag that indicates if the package name should be
#' added to the 'options()' that tells 'downlit' that this is an internal
#' package
#' @param site_url URL of the target site.  It defaults to using the address in
#' the '_quarto.yml' file
#' @export
package_build_documentation <- function(package_source_folder = "",
                                        quarto_sub_folder = "",
                                        quarto_base_folder = here::here(),
                                        convert_readme = TRUE,
                                        convert_news = TRUE,
                                        convert_articles = TRUE,
                                        convert_reference = TRUE,
                                        downlit_options = TRUE,
                                        site_url = get_quarto_entry(quarto_base_folder, "site", "site-url")) {
  
  if (convert_readme | convert_news) msg_color_title("Top files")

  if (convert_readme) {
    package_readme(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (convert_news) {
    package_news(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (convert_articles) {
    package_articles(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder
    )
  }

  if (convert_reference) {
    package_reference(
      package_source_folder = package_source_folder,
      quarto_sub_folder = quarto_sub_folder,
      quarto_base_folder = quarto_base_folder,
      downlit_options = downlit_options,
      site_url = site_url
    )
  }
}

#' Copies the vignettes into Quarto
#' @inheritParams package_build_documentation
#' @param source Sub-folder location where the vignettes are available
#' @param target Sub-folder location to place the convert_articles in Quarto
#' @export
package_articles <- function(package_source_folder = "",
                             source = "vignettes",
                             target = "articles",
                             quarto_sub_folder = "",
                             quarto_base_folder = here::here()) {
  
  msg_color_title("Article files")

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

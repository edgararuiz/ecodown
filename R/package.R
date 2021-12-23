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
                                    branch = "main",
                                    verbosity = c("verbose", "summary", "silent")
                                    ) {

  msg_color_title("Package documentation")

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
    site_url = site_url,
    verbosity = verbosity
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
#' @param verbosity Level of messaging available during run time. Possible values
#' are 'verbose', 'summary', and 'silent'.  Defaults to: 'verbose' 
#' @export
package_build_documentation <- function(package_source_folder = "",
                                        quarto_sub_folder = "",
                                        quarto_base_folder = here::here(),
                                        convert_readme = TRUE,
                                        convert_news = TRUE,
                                        convert_articles = TRUE,
                                        convert_reference = TRUE,
                                        downlit_options = TRUE,
                                        site_url = get_quarto_entry(quarto_base_folder, "site", "site-url"),
                                        verbosity = c("verbose", "summary", "silent")
                                        ) {
  all_files <- dir_ls(package_source_folder, recurse = TRUE, type = "file")
  
  pkg <- pkgdown::as_pkgdown(package_source_folder)
  
  topics <- pkg$topics
  vignettes <- pkg$vignettes
  
  rel_files <- substr(
    all_files, 
    nchar(path_common(all_files)) + 2, 
    nchar(all_files)
  )
  
  file_readme <- all_files[rel_files == "README.md"]
  file_news <- all_files[rel_files == "NEWS.md"]
  
  file_vignettes <- path(
    path_common(all_files), 
    vignettes$file_in
  ) 
  
  file_reference <- path(
    path_common(all_files), "man", 
    topics$file_in[!topics$internal]
  )
  
  file_add <- path()
  if(convert_readme && length(file_readme) > 0) file_add <- c(file_add, file_readme)
  if(convert_news && length(file_news) > 0) file_add <- c(file_add, file_news)
  if(convert_articles && length(file_vignettes) > 0) file_add <- c(file_add, file_vignettes)
  if(convert_reference && length(file_reference) > 0) file_add <- c(file_add, file_reference)
  pkg_files <- as_fs_path(file_add)
  
  file_tree(
    pkg_files, 
    base_folder = package_source_folder, 
    command_name = "package_file",
    addl_entries = list(
      pkg_topics = topics, 
      pkg_vignettes = vignettes, 
      base_folder = path(quarto_base_folder, quarto_sub_folder)
    ),
    verbosity = verbosity[1]
  )
  
}

package_file <- function(input, 
                         base_folder = here::here(),
                         pkg_topics = NULL,
                         pkg_vignettes = NULL
) {
  input_ext <- tolower(path_ext(input))
  input_name <- path_file(input)
  in_folder <- path_file(path_dir(input))
  output_folder <- base_folder
  
  if(tolower(input_name) == "readme.md") {
    output_file <- path(base_folder, "index.md")
  }
  
  if(tolower(input_name) == "news.md") {
    output_file <- path(base_folder, "news.md")
  }  
  
  if(input_ext == "rd" && in_folder == "man") {
    output_name <- path(path_ext_remove(input_name), ext = "md")
    output_folder <- path(base_folder, "reference")
    output_file <- path(output_folder, output_name)
  }
  if(in_folder == "vignettes") {
    output_folder <- path(base_folder, "articles")
    output_file <- path(output_folder, input_name)
    
  }
  if(!dir_exists(output_folder)) dir_create(output_folder)
  if(input_ext == "rd") {
    list_topics <- transpose(pkg_topics)
    input_topic <- list_topics[pkg_topics$file_in == input_name][[1]]
    out <- parse_topic(input_topic)
    writeLines(out, output_file)
  } else {
    file_copy(input, output_file)
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

#' Copies and/or converts files from package source into Quarto
#' @param package_source_folder Path to the package's source code
#' @param quarto_sub_folder Sub folder in `quarto_folder` that will be the base for
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
#' @inheritParams ecodown_build
#' @export
ecodown_convert <- function(package_source_folder = "",
                            quarto_sub_folder = fs::path_file(package_source_folder),
                            quarto_folder = here::here(),
                            convert_readme = TRUE,
                            convert_news = TRUE,
                            convert_articles = TRUE,
                            convert_reference = TRUE,
                            downlit_options = TRUE,
                            site_url = qe(quarto_folder, "site", "site-url"),
                            verbosity = c("verbose", "summary", "silent")) {
  all_files <- dir_ls(package_source_folder, recurse = TRUE, type = "file")

  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  smy <- verbosity == "summary"
  
  msg_color_title("Copying/Converting to Quarto")

  pkg <- pkgdown::as_pkgdown(package_source_folder)

  rel_files <- substr(
    all_files,
    nchar(path_common(all_files)) + 2,
    nchar(all_files)
  )

  file_readme <- all_files[rel_files == "README.md"]
  file_news <- all_files[rel_files == "NEWS.md"]

  vignette_path <- path(path_common(all_files), "vignettes")
  if (dir_exists(vignette_path)) {
    file_vignettes <- dir_ls(
      vignette_path,
      recurse = TRUE,
      type = "file"
    )
  } else {
    file_vignettes <- as.character()
  }

  reference_path <- path(path_common(all_files), "man")
  if (dir_exists(reference_path)) {
    file_reference <- dir_ls(
      reference_path,
      recurse = TRUE,
      type = "file"
    )
  } else {
    file_reference <- as.character()
  }
  
  if (smy && get_package_header() == 0) {
    if(get_clone_header() == 0) msg_summary_title("Copying/Converting to Quarto")
    msg_summary_entry("| R N Art Ref I |\n")
    set_package_header(1)
  }

  qfs <- path(quarto_folder, quarto_sub_folder)

  pf <- path()

  msg_summary_entry("|")

  if (convert_readme && length(file_readme) > 0) {
    pf <- c(pf, file_readme)
    msg_summary_number(length(file_readme))
    if (smy) walk(file_readme, package_file, qfs, pkg)
  } else {
    msg_summary_number(0)
  }

  if (convert_news && length(file_news) > 0) {
    pf <- c(pf, file_news)
    msg_summary_number(length(file_news))
    if (smy) walk(file_news, package_file, qfs, pkg)
  } else {
    msg_summary_number(0)
  }

  if (convert_articles && length(file_vignettes) > 0) {
    pf <- c(pf, file_vignettes)
    msg_summary_number(length(file_vignettes), size = 4)
    if (smy) walk(file_vignettes, package_file, qfs, pkg)
  } else {
    msg_summary_number(0, size = 4)
  }

  if (convert_reference && length(file_reference) > 0) {
    pf <- c(pf, file_reference)
    msg_summary_number(length(file_reference), size = 4)
    if (smy) {
      walk(file_reference, package_file, qfs, pkg)
      ri <- reference_index(
        pkg = pkg,
        quarto_sub_folder = quarto_sub_folder
      )
      writeLines(ri, path(qfs, "reference", "index.md"))
      msg_summary_number(1)
    }
  } else {
    msg_summary_number(0, size = 4)
    msg_summary_number(0)
  }

  msg_summary_entry(" |\n")

  pkg_files <- as_fs_path(pf)

  if (verbosity == "verbose") {
    file_tree(
      pkg_files,
      base_folder = package_source_folder,
      command_name = "package_file",
      addl_entries = list(
        pkg = pkg,
        base_folder = qfs
      ),
      verbosity = "verbose"
    )
    ri <- reference_index(
      pkg = pkg,
      quarto_sub_folder = quarto_sub_folder
    )
    writeLines(ri, path(qfs, "reference", "index.md"))
  }

  downlit_options(
    package = pkg$package,
    url = quarto_sub_folder,
    site_url = site_url
  )
}

package_file <- function(input,
                         base_folder = here::here(),
                         pkg = NULL) {
  
  pkg_topics <- pkg$topics
  
  input_name <- path_file(input)
  
  input_rel <- tolower(substr(
    input,
    nchar(pkg$src_path) + 2,
    nchar(input)
  ))

  input_split <- path_split(input_rel)[[1]]
  
  output_split <- input_split
  if(input_split[[1]] == "man") output_split[[1]] <- "reference"
  if(input_split[[1]] == "vignettes") output_split[[1]] <- "articles"
  li <- length(input_split)
  if(input_split[li] == "readme.md") output_split[li] <- "index.md"
  output_file <- path(
    base_folder, 
    paste0(output_split, collapse = "/")
    )

  output_folder <- path_dir(output_file)
  if (!dir_exists(output_folder)) dir_create(output_folder)
  if (tolower(path_ext(input)) == "rd") {
    list_topics <- transpose(pkg_topics)
    input_topic <- list_topics[pkg_topics$file_in == input_name][[1]]
    out <- reference_parse_topic(input_topic)
    output_file <- path(path_ext_remove(output_file), ext = "md")
    writeLines(out, output_file)
  } else {
    file_copy(input, output_file)
  }
  path_file(output_file)
}

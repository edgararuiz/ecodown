#' Copies and/or converts files from package source into Quarto
#' @param package_source_folder Path to the package's source code
#' @param package_name Name of the package. Defaults to the top folder
#' in the repo URL
#' @param version_folder Folder path to save the documentation version. Examples
#' are: "latest", "dev", "v1.0". Defaults to empty.
#' @param quarto_sub_folder Sub folder in `quarto_folder` that will be the base for
#' the package's documentation.
#' @param convert_readme Flag that indicates if the README file needs to be processed
#' @param convert_news Flag that indicates if the NEWS file needs to be processed
#' @param convert_articles Flag that indicates if the vignette files needs to be processed
#' @param convert_reference Flag that indicates if the help files needs to be processed
#' @param reference_folder The sub folder where the reference files will be placed.
#' Defaults to "reference". 
#' @param vignettes_folder The sub folder where the vignette files will be placed.
#' Defaults to "articles". 
#' @param reference_examples Boolean flag to indicate if the Examples inside the
#' reference page is to be evaluated.
#' @param downlit_options Flag that indicates if the package name should be
#' added to the 'options()' that tells 'downlit' that this is an internal
#' package
#' @param site_url URL of the target site.  It defaults to using the address in
#' the '_quarto.yml' file
#' @param commit Commit to use as the base for the documentation.  It defaults
#' to 'latest_tag'. That default will search for the latest Git tag.  The
#' assumption is that the latest tag is the same as the latest release.  This
#' way we avoid documenting work-in-progress.  The 'latest_commit' value will
#' simply use whatever is cloned. Pass an SHA value if you wish to fix the
#' commit to use.
#' @param branch Repo branch. Defaults to 'main'
#' @inheritParams ecodown_build
#' @export
ecodown_convert <- function(package_source_folder = "",
                            package_name = fs::path_file(package_source_folder),
                            quarto_sub_folder = package_name,
                            version_folder = "",
                            quarto_folder = here::here(),
                            downlit_options = TRUE,
                            site_url = qe(quarto_folder, "site", "site-url"),
                            verbosity = c("verbose", "summary", "silent"),
                            convert_readme = TRUE,
                            convert_news = TRUE,
                            convert_articles = TRUE,
                            convert_reference = TRUE,
                            reference_folder = "reference",
                            vignettes_folder = "articles",
                            reference_examples = TRUE,
                            commit = c("latest_tag", "latest_commit"),
                            branch = "main",
                            reference_output = "qmd",
                            reference_qmd_options = NULL
                            ) {
  
  ecodown_context_set("verbosity", verbosity)
  commit <- commit[1]
  verbosity <- verbosity[1]

  qfs <- path(quarto_folder, quarto_sub_folder, version_folder)

  sha <- checkout_repo(
    pkg_dir = package_source_folder,
    commit = commit,
    branch = branch,
    verbosity = verbosity,
    pkg_name = package_name
  )

  sha_file <- path(qfs, ".ecodown")
  if (file_exists(sha_file)) {
    sha_existing <- readLines(sha_file)
    if (sha_existing == sha) {
      downlit_options(
        package = package_name,
        url = quarto_sub_folder,
        site_url = site_url
      )
      msg_summary_entry("| 0 0   0   0 0 |\n")
      msg_color("Commit already copied...skipping", color = yellow)
      return()
    }
  }

  all_files <- dir_ls(package_source_folder, recurse = TRUE, type = "file")

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

  if(clone_header()) msg_summary_title("Copying/Converting to Quarto")
  msg_summary_header()

  pf <- path()

  msg_summary_entry("|")

  if (convert_readme && length(file_readme) > 0) {
    pf <- c(pf, file_readme)
    msg_summary_number(length(file_readme))
    if (smy) walk(file_readme, package_file, qfs, pkg, reference_folder, vignettes_folder)
  } else {
    msg_summary_number(0)
  }

  if (convert_news && length(file_news) > 0) {
    pf <- c(pf, file_news)
    msg_summary_number(length(file_news))
    if (smy) walk(file_news, package_file, qfs, pkg, reference_folder, vignettes_folder)
  } else {
    msg_summary_number(0)
  }

  if (convert_articles && length(file_vignettes) > 0) {
    pf <- c(pf, file_vignettes)
    msg_summary_number(length(file_vignettes), size = 4)
    if (smy) walk(file_vignettes, package_file, qfs, pkg, reference_folder, vignettes_folder)
  } else {
    msg_summary_number(0, size = 4)
  }

  if (convert_reference && length(file_reference) > 0) {
    pf <- c(pf, file_reference)
    msg_summary_number(length(file_reference), size = 4)
    if (smy) {
      walk(
        file_reference, 
        package_file, 
        qfs, 
        pkg, 
        reference_folder, 
        vignettes_folder, 
        examples = reference_examples,
        output = reference_output,
        output_options = reference_qmd_options
        )
      ri <- reference_index(
        pkg = pkg,
        reference_folder = reference_folder,
        vignettes_folder = vignettes_folder,        
        quarto_sub_folder = quarto_sub_folder,
        version_folder = version_folder, 
        output = reference_output
      )
      
      if(reference_output == "qmd") output_file <- "index.qmd"
      if(reference_output == "md") output_file <- "index.md"
      writeLines(ri, path(qfs, reference_folder, output_file))
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
        base_folder = qfs,
        reference_folder = reference_folder,
        vignettes_folder = vignettes_folder,
        examples = reference_examples
      ),
      verbosity = "verbose"
    )
    ri <- reference_index(
      pkg = pkg,
      reference_folder = reference_folder,
      vignettes_folder = vignettes_folder,
      quarto_sub_folder = quarto_sub_folder,
      version_folder = version_folder
    )
    writeLines(ri, path(qfs, reference_folder, "index.md"))
  }

  writeLines(sha, path(qfs, ".ecodown"))

  downlit_options(
    package = pkg$package,
    url = quarto_sub_folder,
    site_url = site_url
  )
}

package_file <- function(input,
                         base_folder = here::here(),
                         pkg = NULL,
                         reference_folder,
                         vignettes_folder,
                         examples = FALSE,
                         output,
                         output_options) {
  pkg_topics <- pkg$topics

  input_name <- path_file(input)

  abs_input <- path_abs(input)

  input_rel <- tolower(substr(
    abs_input,
    nchar(pkg$src_path) + 2,
    nchar(abs_input)
  ))

  input_split <- path_split(input_rel)[[1]]

  output_split <- input_split
  if (input_split[[1]] == "man") output_split[[1]] <- reference_folder
  if (input_split[[1]] == "vignettes") output_split[[1]] <- vignettes_folder
  li <- length(input_split)
  if (input_split[li] == "readme.md") output_split[li] <- "index.md"
  output_file <- path(
    base_folder,
    paste0(output_split, collapse = "/")
  )
  output_folder <- path_dir(output_file)
  if (!dir_exists(output_folder)) dir_create(output_folder)
  if (tolower(path_ext(input)) == "rd") {
    list_topics <- transpose(pkg_topics)
    input_topic <- list_topics[pkg_topics$file_in == input_name][[1]]
    out <- reference_parse_topic(input_topic, pkg, examples = examples, 
                                 output = output, output_options = output_options
                                 )
    output_file <- path(path_ext_remove(output_file), ext = output)
    writeLines(out, output_file)
  } else {
    file_copy(input, output_file, overwrite = TRUE)
  }
  path_file(output_file)
}

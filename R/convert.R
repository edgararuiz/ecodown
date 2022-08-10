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
#' @param reference_output File type for all the reference files. Possible options
#' are `qmd` and `md`. Defaults to `qmd`.
#' @param reference_qmd_options A character variable that contains the text version
#' of additions to the reference front matter. It applies only to when reference
#' output is `qmd`
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
  
  set_verbosity(verbosity)
  
  commit <- commit[1]
  
  qfs <- path(quarto_folder, quarto_sub_folder, version_folder)

  sha <- checkout_repo(
    pkg_dir = package_source_folder,
    commit = commit,
    branch = branch,
    verbosity = get_verbosity(),
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

  msg_color_title("Copying/Converting to Quarto")

  pkg <- as_pkgdown(package_source_folder)

  rel_files <- substr(
    all_files,
    nchar(path_common(all_files)) + 2,
    nchar(all_files)
  )
  
  if(clone_header()) msg_summary_title("Copying/Converting to Quarto")
  msg_summary_header()
  msg_summary_entry("|")
  
  package_file_args <- list(
    pkg = pkg,
    base_folder = qfs,
    reference_folder = reference_folder,
    vignettes_folder = vignettes_folder,
    examples = reference_examples,
    output = reference_output,
    output_options = reference_qmd_options        
  )
  
  get_files <- function(pkg_folder) {
    p_path <- path(path_common(all_files), pkg_folder)
    x <- as.character()
    if (dir_exists(p_path)) x <- dir_ls(p_path, recurse = TRUE, type = "file")
  }
  
  pf <- path()
  summary_package_files <- function(file_list, summary_width = 2) {
    pf <<- c(pf, file_list)
    msg_summary_number(length(file_list), size = summary_width)
    if (is_summary()) {
      walk(file_list, ~ exec("package_file", !!! package_file_args, input = .x))
    }
  }

  file_readme <- all_files[rel_files == "README.md"]
  file_news <- all_files[rel_files == "NEWS.md"]
  file_vignettes <- get_files("vignettes")
  file_reference <- get_files("man")
  
  if (convert_readme) summary_package_files(file_readme)
  if (convert_news) summary_package_files(file_news)
  if (convert_articles) summary_package_files(file_vignettes)
  if (convert_reference) summary_package_files(file_reference, 4)

  pkg_files <- as_fs_path(pf)

  if (verbosity == "verbose") {
    file_tree(
      pkg_files,
      base_folder = package_source_folder,
      command_name = "package_file",
      addl_entries = package_file_args,
      verbosity = "verbose"
    )
  }
  
  if (convert_reference && length(file_reference) > 0) {
    ri <- reference_index(
      pkg = pkg,
      reference_folder = reference_folder,
      vignettes_folder = vignettes_folder,        
      quarto_sub_folder = quarto_sub_folder,
      version_folder = version_folder, 
      output = reference_output
    )
    writeLines(ri, path(qfs, reference_folder, "index", ext = reference_output))
    msg_summary_number(1)
  } else {
    msg_summary_number(0)
  }
  
  msg_summary_entry(" |\n")

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
    out <- reference_content_default(input_name[[1]], pkg)
    output_file <- path(path_ext_remove(output_file), ext = output)
    writeLines(out, output_file)
  } else {
    file_copy(input, output_file, overwrite = TRUE)
  }
  path_file(output_file)
}

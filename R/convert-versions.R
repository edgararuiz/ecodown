#' Copies and/or converts files from package source into Quarto
#' @param versions A list of additional reference versions to convert.
#' Pass the commit to use, the name of the version, and the sub-folder to use. It
#' expects a named list object. Example:
#' list(list(commit = "latest_commit", path = "dev"))
#' @inheritParams ecodown_convert
#' @export
ecodown_convert_versions <- function(package_source_folder = "",
                                     package_name = fs::path_file(package_source_folder),
                                     quarto_sub_folder = package_name,
                                     version_folder = "",
                                     quarto_folder = here::here(),
                                     downlit_options = FALSE,
                                     site_url = qe(quarto_folder, "site", "site-url"),
                                     verbosity = c("verbose", "summary", "silent"),
                                     convert_readme = TRUE,
                                     convert_news = TRUE,
                                     convert_articles = TRUE,
                                     convert_reference = TRUE,
                                     reference_folder = "reference",
                                     vignettes_folder = "articles",                                     
                                     reference_examples = TRUE,
                                     reference_examples_not_run = FALSE,
                                     reference_output = "qmd",
                                     reference_qmd_options = NULL,   
                                     reference_template = NULL,
                                     branch = "main",
                                     package_description = NULL,
                                     versions = list()) {
  walk(
    versions, ~ {
      ecodown_convert(
        package_source_folder = package_source_folder,
        quarto_sub_folder = quarto_sub_folder,
        quarto_folder = quarto_folder,
        version_folder = .x$version_folder %||% version_folder,
        downlit_options = downlit_options,
        site_url = site_url,
        verbosity = verbosity,
        convert_readme = .x$convert_readme %||% convert_readme,
        convert_news = .x$convert_news %||% convert_news,
        convert_articles = .x$convert_articles %||% convert_articles,
        convert_reference = .x$convert_reference %||% convert_reference,
        reference_folder = .x$reference_folder %||% reference_folder,
        vignettes_folder = .x$vignettes_folder %||% vignettes_folder,
        reference_examples = .x$reference_examples %||% reference_examples,
        reference_output = .x$reference_output %||% reference_output,
        reference_qmd_options = .x$reference_qmd_options %||% reference_qmd_options,
        reference_examples_not_run = .x$reference_examples_not_run %||% reference_examples_not_run,
        commit = .x$commit %||% "latest_tag",
        branch = .x$branch %||% branch,
        reference_template = .x$reference_template %||% reference_template,
        package_description = .x$package_description %||% package_description
      )
    }
  )
}

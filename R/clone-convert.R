#' Clones repo and builds documentation
#' @inheritParams ecodown_convert_versions
#' @inheritParams ecodown_convert
#' @inheritParams ecodown_clone
#' @export
ecodown_clone_convert <- function(repo_url = "",
                                  quarto_sub_folder = path_file(repo_url),
                                  quarto_folder = here::here(),
                                  version_folder = "",
                                  convert_readme = TRUE,
                                  convert_news = TRUE,
                                  convert_articles = TRUE,
                                  convert_reference = TRUE,
                                  reference_folder = "reference",
                                  vignettes_folder = "articles",
                                  downlit_options = TRUE,
                                  site_url = qe(quarto_folder, "site", "site-url"),
                                  commit = c("latest_tag", "latest_commit"),
                                  target_folder = tempdir(),
                                  branch = "main",
                                  reference_examples = TRUE,
                                  verbosity = c("verbose", "summary", "silent"),
                                  versions = list()) {
  set_verbosity(verbosity)

  msg_summary_header()

  pkg_path <- ecodown_clone(
    repo_url = repo_url,
    target_folder = target_folder,
    verbosity = verbosity
  )

  args <- list(
    package_source_folder = pkg_path,
    quarto_sub_folder = quarto_sub_folder,
    version_folder = version_folder,
    quarto_folder = quarto_folder,
    convert_readme = convert_readme,
    convert_news = convert_news,
    convert_articles = convert_articles,
    convert_reference = convert_reference,
    reference_folder = reference_folder,
    vignettes_folder = vignettes_folder,
    downlit_options = downlit_options,
    site_url = site_url,
    branch = branch,
    verbosity = verbosity,
    reference_examples = reference_examples
  )

  if (length(versions) > 0) {
    args <- c(args, list(versions = versions))
    convert_func <- "ecodown_convert_versions"
  } else {
    args <- c(args, list(commit = commit))
    convert_func <- "ecodown_convert"
  }
  exec(convert_func, !!!args)
}

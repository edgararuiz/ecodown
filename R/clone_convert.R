#' Clones repo and builds documentation
#' @inheritParams ecodown_convert
#' @inheritParams ecodown_clone
#' @export
ecodown_clone_convert <- function(repo_url = "",
                                  quarto_sub_folder = path_file(repo_url),
                                  quarto_folder = here::here(),
                                  convert_readme = TRUE,
                                  convert_news = TRUE,
                                  convert_articles = TRUE,
                                  convert_reference = TRUE,
                                  downlit_options = TRUE,
                                  site_url = qe(quarto_folder, "site", "site-url"),
                                  commit = c("latest_tag", "latest_commit"),
                                  target_folder = tempdir(),
                                  branch = "main",
                                  reference_examples = TRUE,
                                  verbosity = c("verbose", "summary", "silent")) {
  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  msg_color_title("Package documentation")

  if (quarto_folder == here::here()) {
    msg_color(bold("quarto_folder: "), here::here(), color = green)
  }

  pkg_name <- path_file(repo_url)
  if (quarto_sub_folder == pkg_name) {
    msg_color(bold("quarto_sub_folder: "), pkg_name, color = green)
  }

  quarto_site <- qe(quarto_folder, "site", "site-url")
  if (site_url == quarto_site) {
    msg_color(bold("site_url: "), quarto_site, color = green)
  }

  if (verbosity == "summary" &&
    get_package_header() == 0 &&
    get_clone_header() == 0
  ) {
    msg_summary_entry("       Clone / Checkout       ")
    msg_summary_entry("| R N Art Ref I |\n")
    set_package_header(1)
    set_clone_header(1)
  }

  pkg_path <- ecodown_clone(
    repo_url = repo_url,
    target_folder = target_folder,
    verbosity = verbosity
  )

  ecodown_convert(
    package_source_folder = pkg_path,
    quarto_sub_folder = quarto_sub_folder,
    quarto_folder = quarto_folder,
    convert_readme = convert_readme,
    convert_news = convert_news,
    convert_articles = convert_articles,
    convert_reference = convert_reference,
    downlit_options = downlit_options,
    site_url = site_url,
    commit = commit,
    branch = branch,
    verbosity = verbosity, 
    reference_examples = reference_examples
  )
}

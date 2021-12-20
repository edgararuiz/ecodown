test_that("Full package documentation works", {
  pkg_location <- test_path("assets/crayon")
  site_folder <- "test_site2"
  temp_dir <- tempdir()
  full_site <- paste0(temp_dir, "/", site_folder)

  expect_output(
    package_build_documentation(
      package_source_folder = pkg_location,
      quarto_sub_folder = site_folder,
      quarto_base_folder = temp_dir,
      site_url = ""
    ),
    "- - - - - - - - Top files - - - - - - - - -"
  )

  expect_equal(
    sort(list.files(full_site)),
    sort(c("articles", "index.md", "NEWS.md", "reference"))
  )

  expect_output(
    site_autolink_html(
      quarto_base_folder = test_path("assets/crayon-html"),
      render_folder = NULL
    ),
    "- - - - - - Auto-linking - - - - - - - - -"
  )

  expect_output(
    package_file_copy(file_names = "test"),
    "not found: test"
  )

  expect_output(
    package_build_documentation(
      package_source_folder = test_path("assets/crayon2"),
      quarto_sub_folder = site_folder,
      quarto_base_folder = temp_dir
    ),
    "- - - - - - - - Top files - - - - - - - - -"
  )

  expect_equal(
    get_quarto_entry(test_path("assets"), "site", "title"),
    "testsite"
  )
})

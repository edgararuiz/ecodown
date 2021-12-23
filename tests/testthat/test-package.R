test_that("Full package documentation works", {
  pkg_location <- test_path("assets/crayon")
  site_folder <- "test_site2"
  temp_dir <- tempdir()
  full_site <- paste0(temp_dir, "/", site_folder)

  expect_output(
    ecodown_convert(
      package_source_folder = pkg_location,
      quarto_sub_folder = site_folder,
      quarto_folder = temp_dir,
      site_url = ""
    ),
    msg_title_raw("Top files")
  )

  expect_equal(
    sort(list.files(full_site)),
    sort(c("articles", "index.md", "NEWS.md", "reference"))
  )

  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/crayon-html"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    msg_title_raw("Auto-linking")
  )

  expect_output(
    package_file_copy(file_names = "test"),
    "not found: test"
  )

  expect_output(
    ecodown_convert(
      package_source_folder = test_path("assets/crayon2"),
      quarto_sub_folder = site_folder,
      quarto_folder = temp_dir
    ),
    msg_title_raw("Top files")
  )

  expect_equal(
    quarto_entry(test_path("assets"), "site", "title"),
    "testsite"
  )
})

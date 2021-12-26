test_that("Full package documentation works", {
  
  capture.output(
    ecodown_convert(
      package_source_folder = test_path("assets/crayon"),
      quarto_sub_folder = "test1",
      quarto_folder = tempdir(),
      site_url = ""
    )
  )

  expect_equal(
    sort(path_file(dir_ls(path(tempdir(), "test1")))),
    sort(c("articles", "index.md", "news.md", "reference"))
  )

  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/crayon-html"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    "Auto-linking"
  )

  capture.output(
    ecodown_convert(
      package_source_folder = test_path("assets/crayon2"),
      quarto_sub_folder = "test2",
      quarto_folder = tempdir()
    )  
  )
  
  expect_equal(
    sort(path_file(dir_ls(path(tempdir(), "test2")))),
    sort(c("index.md", "news.md", "reference"))
  )
  expect_equal(
    qe(test_path("assets"), "site", "title"),
    "testsite"
  )
})

test_that("Full package documentation works", {
  
  site_folder <- "tidypredict_site"
  temp_dir <- tempdir()
  
  expect_silent(
    pkg_location <- system.file(package = "ecodown", "testutils/tidypredict")
  )
  
  expect_output(
    package_build_documentation(
      pkg_folder = pkg_location, 
      project_folder = site_folder,
      root_folder = temp_dir
    ),
    "- - - - - - - - Top files - - - - - - - - -"
  )

  expect_equal(
    list.files(paste0(temp_dir, "/tidypredict_site")),
    c("articles", "index.md", "news.md", "reference")
  )
})
test_that("Full package documentation works", {
  
  pkg_location <- test_path("assets/tidypredict")
  
  site_folder <- "test_site"
  
  temp_dir <- tempdir()
  
  expect_output(
    package_build_documentation(
      pkg_folder = pkg_location, 
      project_folder = site_folder,
      root_folder = temp_dir
    ),
    "- - - - - - - - Top files - - - - - - - - -"
  )
  
  expect_output(
    site_autolink_html(
      quarto_folder = path(temp_dir, site_folder)
    ),
    "- - - - - - Auto-linking - - - - - - - - -"
  )

  expect_equal(
    list.files(paste0(temp_dir, "/", site_folder)),
    c("articles", "index.md", "news.md", "reference")
  )

})
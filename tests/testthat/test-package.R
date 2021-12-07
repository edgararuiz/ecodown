test_that("Full package documentation works", {
  
  site_folder <- "tidypredict_site"
  temp_dir <- tempdir()
  pkg_folder <- "testutils/tidypredict"
  
  location_inst <- system.file(package = "ecodown", pkg_folder)
  location_source <- paste0("inst/", pkg_folder)
  
  if(dir.exists(location_source)) {
    pkg_location <- location_source
  } else {
    pkg_location <- location_inst
  }
  
  
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
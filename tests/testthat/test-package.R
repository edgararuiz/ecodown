test_that("Full package documentation works", {
  
  site_folder <- "tidypredict_site"
  temp_dir <- tempdir()
  
  location_inst <- system.file(package = "ecodown", "testutils/tidypredict")
  location_source <- "inst/testutils/tidypredict"
  
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

  expect_equal(
    list.files(paste0(temp_dir, "/tidypredict_site")),
    c("articles", "index.md", "news.md", "reference")
  )
})
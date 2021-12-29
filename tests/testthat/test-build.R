test_that("Build site works", {
  
  dir_site <- path(tempdir(), "build1")
  
  dir_create(dir_site)
  
  lapply(
    dir_ls(test_path("assets/quarto_files")), 
    function(x) file_copy(x, dir_site, overwrite = TRUE)
    )

  capture.output(
    ecodown_build(quarto_folder = dir_site)
  )
  
  top_files <- path_file(dir_ls(dir_site))
  
  expect_true("index.md" %in% top_files)
  expect_true("_quarto.yml" %in% top_files)
  expect_true("mleap" %in% top_files)
  
})

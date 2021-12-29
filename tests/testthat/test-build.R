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
  
  expect_equal(
    sort(path_file(dir_ls(dir_site))),
    c("_ecodown.yml", "_quarto.yml", "docs", "index.md", "mleap")
  )
})

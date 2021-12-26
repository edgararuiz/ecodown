test_that("Build site works", {
  
  dir_site <- paste0(tempdir(), "/build_site")
  if (dir.exists(dir_site)) unlink(dir_site, recursive = TRUE, force = TRUE)
  dir.create(dir_site)
  file_utils <- test_path("assets/quarto_files")
  lapply(dir_ls(file_utils), function(x) file_copy(x, dir_site, overwrite = TRUE))

  capture.output(
    ecodown_build(quarto_folder = dir_site)
  )
  
  expect_equal(
    sort(path_file(dir_ls(dir_site))),
    c("_ecodown.yml", "_quarto.yml", "index.md", "mleap")
  )
})

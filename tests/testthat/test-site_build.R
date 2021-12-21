test_that("Build site works", {
  dir_site <- paste0(tempdir(), "/build_site")
  if (dir.exists(dir_site)) unlink(dir_site, recursive = TRUE, force = TRUE)
  dir.create(dir_site)
  file_utils <- test_path("assets/quarto_files")
  lapply(dir_ls(file_utils), function(x) file_copy(x, dir_site, overwrite = TRUE))

  expect_output(
    site_build_quarto(quarto_base_folder = dir_site),
    "- - - - - - - - - Init - - - - - - - - - - -"
  )

  expect_equal(
    length(list.dirs(dir_site, recursive = FALSE)),
    2
  )
})

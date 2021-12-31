test_that("Default cloning works", {
  unlink(path(tempdir(), "ecodown"), recursive = TRUE, force = TRUE)
  skip_if_offline("github.com")

  t_folder <- path(tempdir(), "clone1")
  dir_create(t_folder)

  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/edgararuiz/ecodown",
      target_folder = t_folder,
      quarto_folder = path(tempdir(), "/site1"),
      quarto_sub_folder = "package1"
    )
  )

  top_files <- path_file(dir_ls(path(tempdir(), "site1", "package1")))

  #expect_true("articles" %in% top_files)
  expect_true("index.md" %in% top_files)
  #expect_true("news.md" %in% top_files)
  expect_true("reference" %in% top_files)
  
})

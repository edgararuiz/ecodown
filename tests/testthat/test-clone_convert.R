test_that("Default cloning works", {
  skip_if_offline("github.com")
 
  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/tidyverse/purrr",
      target_folder = paste0(tempdir(), "/clone1"),
      quarto_folder = paste0(tempdir(), "/site1"),
      quarto_sub_folder = "purrr1"
    )    
  )
  
  top_files <- path_file(dir_ls(path(tempdir(), "site1", "purrr1"))) 
  
  expect_true("articles" %in% top_files)
  expect_true("index.md" %in% top_files)
  expect_true("news.md" %in% top_files)
  expect_true("reference" %in% top_files)
  
})

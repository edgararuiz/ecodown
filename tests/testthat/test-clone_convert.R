test_that("Default cloning works", {
  skip_if_offline("github.com")
 
  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/r-lib/crayon",
      target_folder = paste0(tempdir(), "/clone1"),
      quarto_folder = paste0(tempdir(), "/crayon1"),
      quarto_sub_folder = "crayon"
    )    
  )
  
  expect_equal(
    length(dir_ls(path(tempdir(), "crayon1", "crayon"))),
    3
  )
  
})

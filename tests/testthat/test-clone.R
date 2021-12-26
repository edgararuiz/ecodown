pkg_url <- "https://github.com/r-lib/crayon"
pkg_commit <- "80bfc4c90da668a77da1410c037181d097de3354"

test_that("Default cloning works", {
  skip_if_offline("github.com")

  capture.output(
    pkg_path <- ecodown_clone(pkg_url)  
  )
  
  expect_equal(
    length(list.files(pkg_path)),
    10
  )
  
  expect_output(
    checkout_commit(pkg_path, pkg_commit),
    "- Checking out SHA: 80bfc4c..."
  )

  capture.output(
    ecodown_clone_convert(
      repo_url = pkg_url,
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

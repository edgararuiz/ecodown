pkg_url <- "https://github.com/r-lib/crayon"
pkg_commit <- "80bfc4c90da668a77da1410c037181d097de3354"

test_that("Default cloning works",{
  skip_if_offline("github.com")
  
  expect_output(
    pkg_path <- package_clone_git_repo(pkg_url),
    "- - - - - - - Cloning repo - - - - - - - -"
  )

  expect_output(
    checkout_commit(pkg_path, pkg_commit),
    "- Checking out SHA: 80bfc4c..."
  )
  
  expect_output(
    package_clone_and_build(
      url = pkg_url, 
      target = paste0(tempdir(), "/newtest"),
      project_folder = "crayon"
      ),
    "- - - - - - - Cloning repo - - - - - - - -"
  )
  
})
test_that("Default cloning works", {
  skip_if_offline("github.com")

  capture.output(
    pkg_path <- ecodown_clone("https://github.com/r-lib/crayon")  
  )
  
  expect_equal(
    length(list.files(pkg_path)),
    10
  )
  
  expect_output(
    checkout_commit(
      pkg_path, 
      "80bfc4c90da668a77da1410c037181d097de3354"
      ),
    "- Checking out SHA: 80bfc4c..."
  )
   
})

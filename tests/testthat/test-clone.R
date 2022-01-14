test_that("Default cloning works", {
  skip_if_offline("github.com")

  capture.output(
    pkg_path <- ecodown_clone("https://github.com/r-lib/crayon", 
                              verbosity = "summary"
                              )  
  )
  
  expect_equal(
    length(list.files(pkg_path)),
    10
  )
  
  x <- capture.output(
    checkout_commit(
      pkg_path, 
      "80bfc4c90da668a77da1410c037181d097de3354",
      ck_type = "sha", tag = ""
    )
  )
  
  expect_equal(
    substr(x, 1, 25),
    "Checking out SHA: 80bfc4c"
  )
   
})

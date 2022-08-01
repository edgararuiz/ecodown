test_that("Default cloning works", {
  skip_if_offline("github.com")

  capture.output(
    pkg_path <- ecodown_clone(
      "https://github.com/r-lib/crayon",
      verbosity = "summary"
    )
  )

  re_clone <- capture.output(
    ecodown_clone(
      "https://github.com/r-lib/crayon",
      target_folder = path_dir(pkg_path),
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

  expect_equal(pkg_path, path(re_clone))

  expect_equal(x, "[1] \"80bfc4c90da668a77da1410c037181d097de3354\"")
})

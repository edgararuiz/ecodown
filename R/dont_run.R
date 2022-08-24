#' Extracts all "don't run" examples in a package
#' @inheritParams dont_run_test
#' @export
dont_run_get <- function(pkg) {
  pkg %>% 
    check_all_reference(silent = TRUE) %>% 
    map(~ .x$examples$code_dont_run) %>% 
    discard(is.null)
}

#' Copies "don't run" examples from package into individual test scripts and
#' executes the tests via `testthat` 
#' @param pkg A `pkgdown` package object, or a character path to the location of
#' the package to parse.
#' @param test_dir Path to where to save the temp test files
#' @param testthat_reporter `testthat` reporter to use when running tests
#' @export 
dont_run_test <- function(pkg, test_dir = NULL, testthat_reporter = testthat::SummaryReporter) {
  if(is.character(pkg)) pkg <- as_pkgdown(pkg)
  if(is.null(test_dir)) {
    test_dir <- path(tempdir(), paste0(pkg$package, "-tests"))
  }
  if(!dir_exists(test_dir)) dir_create(test_dir)
  
  dr <- dont_run_get(pkg)
  
  purrr::iwalk(dr, ~ {
    fp <- path(test_dir,paste0("test-", path_ext_remove(.y)), ext = "R")
    writeLines(.x, fp)
  })
  
  dir_tree(test_dir)
  
  testthat::test_dir(test_dir, reporter = testthat_reporter)
}
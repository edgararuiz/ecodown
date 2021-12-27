test_that("Auto-linking works", {
  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/crayon-html"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    "Auto-linking"
  )
})

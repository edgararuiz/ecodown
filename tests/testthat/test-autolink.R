test_that("Auto-linking works", {
  options("downlit.attached" = "crayon")
  options("downlit.local_packages" = "crayon")
  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/crayon_html"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    "Auto-linking"
  )

  options("downlit.attached" = NULL)
  options("downlit.local_packages" = NULL)
  expect_error(
    ecodown_autolink(
      quarto_folder = test_path("assets/crayon_html"),
      render_folder = NULL
    )
  )

  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/quarto_files"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    "Auto-linking"
  )

  expect_output(
    ecodown_autolink(
      quarto_folder = test_path("assets/callthat_files"),
      render_folder = NULL,
      verbosity = "verbose"
    ),
    "Auto-linking"
  )
})

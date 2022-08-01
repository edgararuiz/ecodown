test_that("Quarto render works", {
  quarto_path <- test_path("assets/testsite")
  
  ecodown_quarto_render(
    quarto_folder = quarto_path
  )
  
  expect_true(
    file_exists(path(quarto_folder, "_site", "index.html"))
  )
  
})

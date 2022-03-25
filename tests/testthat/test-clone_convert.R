test_that("Default cloning works", {
  skip_if_offline("github.com")

  t_folder <- path(tempdir(), "clone1")
  dir_create(t_folder)

  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/edgararuiz/ecodown",
      target_folder = t_folder,
      quarto_folder = path(tempdir(), "site1"),
      quarto_sub_folder = "package1", 
      verbosity = "summary",
      reference_examples = FALSE
    )
  )
  
  site_dir <- path(tempdir(), "site1", "package1")
  

  top_files <- path_file(dir_ls(site_dir, all = TRUE))

  expect_true(".ecodown" %in% top_files)
  expect_true("index.md" %in% top_files)
  expect_true("reference" %in% top_files)

  original_time <- file.info(path(site_dir, ".ecodown"))$mtime
  
  t_folder3 <- path(tempdir(), "clone3")
  dir_create(t_folder3)
  
  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/edgararuiz/ecodown",
      target_folder = t_folder3,
      quarto_folder = path(tempdir(), "site3"),
      quarto_sub_folder = "package3", 
      verbosity = "summary",
      reference_examples = FALSE
    )
  )
  
  re_convert <- capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/edgararuiz/ecodown",
      target_folder = t_folder3,
      quarto_folder = path(tempdir(), "site3"),
      quarto_sub_folder = "package3", 
      verbosity = "summary",
      reference_examples = FALSE
    )
  )
  
  expect_equal(re_convert[1], "ecodown (Latest)              | 0 0   0   0 0 |")
  
  new_time <- file.info(path(site_dir, ".ecodown"))$mtime
  
  expect_equal(original_time, new_time)
})

test_that("Convert multiple versions", {
  skip_if_offline("github.com")
  
  t_folder2 <- path(tempdir(), "clone2")
  dir_create(t_folder2)
  
  site_dir <- path(tempdir(), "site2", "package2")
  
  capture.output(
    ecodown_clone_convert(
      repo_url = "https://github.com/edgararuiz/ecodown",
      target_folder = t_folder2,
      quarto_folder = path(tempdir(), "site2"),
      quarto_sub_folder = "package2", 
      verbosity = "verbose",
      reference_examples = FALSE,
      versions = list(
        list(folder = "spec", 
             commit = "f92d3065e386740a19ee5a621619c8d48793459e"
        ),
        list(folder = "new",
             commit = "9751da8828c4bab0232c35ffd36928bfcb564372")
      )
    )
  )
  
  
  top_files <- path_file(dir_ls(site_dir, all = TRUE))
  
  expect_true(".ecodown" %in% top_files)
  expect_true("index.md" %in% top_files)
  expect_true("reference" %in% top_files)
})


file_tree <- function(file_list,
                      file_type = "",
                      base_folder,
                      command_name = "",
                      entry_value = NULL,
                      addl_entries = list(),
                      verbosity = "summary") {
  tree_start <- Sys.time()
  file_unique <- unique(path_dir(file_list))
  file_sort <- sort(file_unique)
  rel_sort <- substr(
    file_sort,
    nchar(path_dir(base_folder)) + 2,
    nchar(file_sort)
  )
  for (i in seq_along(rel_sort)) {
    matched_files <- path_dir(file_list) == file_sort[i]
    no_files <- sum(matched_files)
    no_caption <- ifelse(no_files > 1, "files", "file")
    curr_sort <- rel_sort[i]
    pss <- ""
    if (i > 1) {
      pc <- path_common(rel_sort[c(i, i - 1)])
      fln <- path_file(curr_sort)
      ps <- path_split(curr_sort)[[1]]
      pss <- paste0(rep("|-- ", times = length(ps) - 1), collapse = "")
      psc <- silver(pss)
      flc <- black(fln)
      # curr_sort <- paste0(psc, flc)
    }
    no_cat <- magenta(paste0(" (", no_files, " ", file_type, no_caption, ")"))
    cat_msg <- paste0(curr_sort, no_cat)
    if (verbosity == "verbose") cat_msg <- paste0(cat_msg, "\n")
    section_start <- Sys.time()
    cat(cat_msg)
    if (command_name != "") {
      walk(
        file_list[matched_files],
        ~ {
          start_time <- Sys.time()
          if (verbosity == "verbose") {
            cat(paste0(silver(paste0(pss, "|-- ")), path_file(.x)))
          }
          res <- exec_command(
            command_name = command_name,
            entry_value = entry_value,
            addl_entries = c(addl_entries, list(input = .x))
          )
          res_msg <- ""
          if (!is.null(res)) {
            if (path_file(res) != path_file(.x)) res_msg <- blue(" =>", res)
          }
          stop_time <- Sys.time()
          if (verbosity == "verbose") {
            doc_time <- cat_time(start_time, stop_time)
            cat(paste0(res_msg, silver(doc_time), "\n"))
          }
        }
      )
    }
    section_end <- Sys.time()
    section_time <- cat_time(section_start, section_end)
    if (verbosity == "summary") cat(paste0(section_time, "\n"))
  }
  sep_cat <- paste0(rep("=", times = 46), collapse = "")
  cat(silver(sep_cat, "\n"))

  tree_time <- cat_time(tree_start, Sys.time(), add_brackets = FALSE)

  cat(silver("Total files: ", length(file_list), " ---- Total time: ", tree_time, "\n"))
}

cat_time <- function(start_time = Sys.time() - 600,
                     end_time = Sys.time(),
                     add_brackets = TRUE) {
  time_length <- end_time - start_time
  length_value <- time_length[[1]]
  round_value <- round(length_value, 1)
  time_unit <- substr(attr(time_length, "units"), 1, 1)
  if (add_brackets) {
    paste0(" [ ", round_value, time_unit, " ]")
  } else {
    paste0(round_value, time_unit)
  }
}

exec_command <- function(command_name = "",
                         entry_value = NULL,
                         addl_entries = list()) {
  x <- FALSE
  if (!is.null(entry_value)) {
    if (entry_value != FALSE) {
      x <- TRUE
    }
  } else {
    x <- TRUE
  }

  if (x) {
    entries <- c(entry_value, addl_entries)
    out <- exec(command_name, !!!entries)
  }
  out
}

file_tree <- function(file_list, 
                      file_type = "", 
                      base_folder,
                      command_name = "", 
                      entry_value = NULL, 
                      addl_entries = list()
) {
  file_unique <- unique(path_dir(file_list))
  file_sort <- sort(file_unique)
  rel_sort <- substr(
    file_sort,
    nchar(path_dir(base_folder)) + 2,
    nchar(file_sort)
  )
  for(i in seq_along(rel_sort)) {
    matched_files <- path_dir(file_list) == file_sort[i]
    no_files <- sum(matched_files)
    no_caption <- ifelse(no_files > 1, "files", "file")
    curr_sort <- rel_sort[i]
    pss <- ""
    if(i > 1) {
      pc <- path_common(rel_sort[c(i, i-1)])
      fln <- path_file(curr_sort)
      ps <- path_split(curr_sort)[[1]]
      pss <- paste0(rep("|--- ", times = length(ps) - 1), collapse = "")
      psc <- silver(pss)
      flc <- black(fln)
      curr_sort <- paste0(psc, flc)
    }
    no_cat <- magenta(paste0(" (", no_files, " ", file_type, no_caption, ")"))
    cat_msg <- paste0(curr_sort, no_cat, "\n")
    cat(cat_msg) 
    if(command_name != "") {
      walk(
        file_list[matched_files],
        ~{
          exec_command(
            command_name = command_name, 
            entry_value = entry_value, 
            addl_entries = c(addl_entries, list(input = .x))
          )
          if(get_verbosity() == "verbose") {
            cat(paste0(silver(paste0(pss, "|--- ")), path_file(.x), "\n"))
          } 
        } 
      )
    }
  }
  sep_cat <- paste0(rep("=", times = 46), collapse = "")
  cat(silver(sep_cat, "\n"))
  cat(silver("Total files: ", length(file_list), "\n"))
}

exec_command <- function(command_name = "", 
                         entry_value = NULL, 
                         addl_entries = list()
) {
  x <- FALSE
  if(!is.null(entry_value)) {
    if(entry_value != FALSE) {
      x <- TRUE
    }
  } else {
    x <- TRUE
  }
  
  if(x) {
    entries <- c(entry_value, addl_entries)
    exec(command_name, !!! entries)  
  }
  
}
full_file_copy <- function(folder, new_folder, exclude_exts = NULL) {
  if (!dir_exists(new_folder)) dir_create(new_folder)
  fls <- sanitized_file_list(
    folder = folder,
    exclude_exts = exclude_exts
  )
  fd <- folder_list(fls)
  dir_create(path(new_folder, fd))
  walk(fls, ~ file_copy(path(folder, .x), path(new_folder, .x), overwrite = TRUE))
}

folder_list <- function(file_list) {
  dj <- as_fs_path(unique(path_dir(file_list)))
  dj[dj != "."]
}

full_file_list <- function(folder, exclude_exts = NULL) {
  fc <- dir_ls(folder, recurse = TRUE, all = TRUE)
  if (!is.null(exclude_exts)) {
    for (i in 1:length(exclude_exts)) {
      fc <- fc[path_ext(fc) != exclude_exts[i]]
    }
  }
  fc
}

sanitized_file_list <- function(folder, exclude_exts = NULL) {
  fc <- full_file_list(
    folder = folder,
    exclude_exts = exclude_exts
  )
  pl <- length(path_split(path_common(fc))[[1]])
  fcs <- path_split(fc)
  rf <- map(fcs, ~ .x[(pl + 1):length(.x)])
  rj <- path_join(rf)
  rj[is_file(fc)]
}

create_folder_if_missing <- function(x) {
  if (!dir_exists(x)) dir_create(x)
}

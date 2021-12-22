#' Get entry from the Quarto setup file
#' @param quarto_path Path to the Quarto folder, defaults to project path.
#' @param ... A single or multiple entries with the path to the desired entry.
#' @param file_name The name of the Quarto setup file, '_quarto.yml'
#' @export
get_quarto_entry <- function(quarto_path = here::here(), ..., file_name = "_quarto.yml") {
  if (!file_exists(path(quarto_path, file_name))) {
    return("")
  }
  element <- list(...)
  cont <- read_yaml(path(quarto_path, file_name))
  for (i in seq_along(element)) {
    cont <- pluck(cont, element[[i]])
  }
  if (is.null(cont)) cont <- ""
  cont
}

create_folder_if_missing <- function(x) if (!dir_exists(x)) dir_create(x)

msg_color <- function(..., color = black) {
  if(get_verbosity() == "verbose") {
    cat(color(paste0("- ", ..., "\n")))   
  }
}

msg_color_bold <- function(..., color = black) {
  if(get_verbosity() == "verbose") {
    cat(bold(color(paste0("- ", ..., "\n")))) 
  }
}

msg_color_title <- function(..., color = blue) {
  if(get_verbosity() == "verbose") {
    title <- msg_title_raw(paste0(...))
    cat(bold(color(paste0(title, "\n"))))
  }
}

msg_title_raw <- function(title) {
  n_side <- floor(40 - nchar(title) / 2)
  sides <- paste0(rep("- ", times = n_side / 2), collapse = "")
  x <- paste0(sides, title, " ", sides)
  substr(x, 1, 78)
}

msg_summary_entry <- function(x, color = black) {
  if(get_verbosity() == "summary") {
    cat(color(x))
  }
}

msg_summary_title <- function(x) {
  if(get_verbosity() == "summary") {
    x <- paste0(">> ", x, "\n")
    cat(black(bold(x)))
  }
}

msg_summary_number <- function(x, size = 2, color = black, side = c("left", "right")) {
  if(get_verbosity() == "summary") {
    side <- side[[1]]
    x <- as.character(x)
    xn <- nchar(x)
    
    pad <- paste0(rep(" ", times = (size - xn)), collapse = "")
    if(side == "left") cat(color(paste0(pad, x)))
    if(side == "right") cat(color(paste0(x, pad)))
  }
}

msg_summary_tree <- function(file_list, file_type = "", base_folder) {
  if(get_verbosity() == "summary") {
    file_tree(
      file_list = file_list,
      file_type = file_type,
      base_folder = base_folder
    )
  }
}

ecodown_context <- new.env(parent = emptyenv())

ecodown_context_set <- function(id, vals = list()) {
  ecodown_context[[id]] <- vals
}

ecodown_context_get <- function(id) {
  if (id == "") return(NULL)
  ecodown_context[[id]]
}

get_verbosity <- function() {
  x <- ecodown_context_get("verbosity")
  if(is.null(x)) x <- "verbose"
  x[[1]]
} 

file_tree <- function(file_list, file_type = "", base_folder) {
  file_unique <- unique(path_dir(file_list))
  file_sort <- sort(file_unique)
  rel_sort <- substr(
    file_sort,
    nchar(path_dir(base_folder)) + 2,
    nchar(file_sort)
  )
  for(i in seq_along(rel_sort)) {
    no_files <- sum(path_dir(file_list) == file_sort[i])
    no_caption <- ifelse(no_files > 1, "files", "file")
    curr_sort <- rel_sort[i]
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
  }
  sep_cat <- paste0(rep("=", times = 46), collapse = "")
  cat(silver(sep_cat, "\n"))
  cat(silver("Total files: ", length(file_list), "\n"))
}

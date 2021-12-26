msg_color <- function(..., color = black) {
  if (get_verbosity() == "verbose") {
    cat(color(paste0("- ", ..., "\n")))
  }
}

msg_color_bold <- function(..., color = black) {
  if (get_verbosity() == "verbose") {
    cat(bold(color(paste0("- ", ..., "\n"))))
  }
}

msg_color_title <- function(x, color = blue) {
  if (get_verbosity() == "verbose") {
    # title <- msg_title_raw(paste0(...))
    # cat(bold(color(paste0(title, "\n"))))
    x <- paste0(">> ", x, "\n")
    cat(black(bold(x)))
  }
}

msg_title_raw <- function(title) {
  n_side <- floor(40 - nchar(title) / 2)
  sides <- paste0(rep("- ", times = n_side / 2), collapse = "")
  x <- paste0(sides, title, " ", sides)
  substr(x, 1, 78)
}

msg_summary_entry <- function(x, color = black) {
  if (get_verbosity() == "summary") {
    cat(color(x))
  }
}

msg_summary_title <- function(x) {
  if (get_verbosity() == "summary") {
    x <- paste0(">> ", x, "\n")
    cat(black(bold(x)))
  }
}

msg_summary_number <- function(x, size = 2, color = black, side = c("left", "right")) {
  if (get_verbosity() == "summary") {
    side <- side[[1]]
    x <- as.character(x)
    xn <- nchar(x)

    pad <- paste0(rep(" ", times = (size - xn)), collapse = "")
    if (side == "left") cat(color(paste0(pad, x)))
    if (side == "right") cat(color(paste0(x, pad)))
  }
}

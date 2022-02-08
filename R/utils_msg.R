msg_color <- function(..., color = black, return = TRUE) {
  if (get_verbosity() == "verbose") {
    msg <- paste0(blue(bold("i ")), color(..., ifelse(return, "\n", "") ))
    cat(msg)
  }
}

msg_color_line <- function(..., color = black, return = FALSE) {
  if (get_verbosity() == "verbose") {
    cat(color(paste0(..., ifelse(return, "\n", "") )))
  }
}

msg_color_title <- function(x, color = blue) {
  if (get_verbosity() == "verbose") {
    x <- paste0(">> ", x, "\n")
    cat(black(bold(x)))
  }
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

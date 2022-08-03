reference_parse_tag <- function(x) {
  tg_res <- map(x, ~ {
    lv1 <- .x
    if (length(lv1) > 0) {
      if (length(lv1) == 1) {
        res <- reference_parse_line_tag(lv1)
      } else {
        lv2 <- map(lv1, reference_parse_line_tag)
        res <- paste0(lv2, collapse = "")
      }
      if ("tag_code" %in% class(lv1)) res <- paste0("`", res, "`")
      if ("tag_dontrun" %in% class(lv1)) res <- paste0("```r\n", res, "\n```")
    } else {
      res <- ""
    }
    res
  })
  if (all(map_lgl(x, ~ "RCODE" %in% class(.x)))) {
    tg_res <- c("```r", tg_res, "```")
  }
  paste0(tg_res, collapse = "")
}

reference_parse_line_tag <- function(x) {
  tg_res <- map(x, ~ {
    if (length(.x) > 0) {
      res <- as.character(.x)
      if ("RCODE" %in% class(.x)) res <- paste0("`", res, "`")
    } else {
      res <- ""
    }
    res
  })
  if ("tag_item" %in% class(x)) tg_res <- "\n* "
  paste0(tg_res, collapse = "")
}

code_run <- function(x) {
  res <- NULL
  for (i in seq_along(x)) {
    cl <- x[i]
    cls <- cl
    cr <- substr(cl, nchar(cl) - 1, nchar(cl)) == "\n"
    if (cr) cl <- substr(cl, 1, nchar(cl) - 2)
    res <- c(res, cl)
    if (cl != "") {
      if (substr(cl, 1, 1) != "#") {
        out <- capture.output(eval(parse_expr(cl)))
        out1 <- paste0("#> ", out)
        out2 <- paste0(out1, collapse = "\n")
        if (length(out) > 0) res <- c(res, paste0(out2, "\n"))
      }
    }
  }
  p_res <- paste0(res, collapse = "")
  paste0("```r\n", p_res, "\n```")
}

reference_arguments <- function(x) {
  args_p <- map_chr(x[[1]], ~ {
    et <- .x
    ec <- paste0(map(et, reference_parse_tag), collapse = " | ")
  })
  args_ret <- map_lgl(args_p, ~ .x != "\n")
  args_filter <- args_p[args_ret]
  map_chr(args_filter, ~ paste0(strsplit(.x, "\n")[[1]], collapse = " "))
}
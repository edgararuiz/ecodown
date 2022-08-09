reference_parse <- function(file_in, pkg) {
  file_in %>%
    reference_get_tags(pkg) %>%
    reference_get_sections()
}

reference_get_tags <- function(file_in, pkg) {
  pkg_topics <- pkg$topics
  topic_row <- pkg_topics[pkg_topics$file_in == file_in, ]
  topic <- transpose(topic_row)
  topic_rd <- topic[[1]]$rd
  tag_names <- map_chr(topic_rd, ~ class(.)[[1]])
  tag_split <- split(topic_rd, tag_names)
  tag_split <- tag_split[names(tag_split) != "COMMENT"]
  tag_split <- tag_split[names(tag_split) != "TEXT"]
  tag_split
}

reference_get_sections <- function(tags) {
  set_names(
    map(tags, reference_tag),
    substr(names(tags), 5, nchar(names(tags)))
  )
}

reference_tag <- function(x) {
  sub_x <- x[[1]]
  res <- NULL
  if ("tag_examples" %in% class(sub_x)) {
    dontrun_flags <- map_lgl(sub_x, ~ "tag_dontrun" %in% class(.x))
    w_dontrun <- which(dontrun_flags)
    if (length(w_dontrun) > 0) {
      code_run_tags <- sub_x[1:w_dontrun - 1]
      code_dont_run_tags <- sub_x[dontrun_flags][[1]]
      code_run <- reference_tag_example(code_run_tags)
      code_dont_run <- reference_tag_example(code_dont_run_tags)
      res <- list(
        code_run = code_run,
        code_dont_run = code_dont_run
      )
    } else {
      res <- list(
        code_run = reference_tag_example(sub_x),
        code_dont_run = NULL
      )
    }
  }

  if ("tag_arguments" %in% class(sub_x)) {
    res <- list()
    for (i in seq_along(sub_x)) {
      item <- sub_x[[i]]
      if ("tag_item" %in% class(item)) {
        arg <- reference_single_tag(item[[1]])
        desc <- reference_tag_lvl2(item[[2]], "")
        res <- c(res, list(c(arg, desc)))
      }
    }
  }

  if ("tag_usage" %in% class(sub_x)) {
    res <- map(x, ~ map(.x, reference_tag_lvl2))
    #res <- res[res != ""]
    res <- flatten(res)
  }
  
  if (is.null(res)) {
    curr_res <- NULL
    for (i in seq_along(sub_x)) {
      out <- reference_tag_lvl1(sub_x[[i]])
      if ("tag_itemize" %in% class(sub_x[[i]])) {
        res <- c(res, curr_res, out)
        curr_res <- NULL
      }
      #curr_res <- paste0(curr_res, out, collapse = "")
      if(out == "\n") {
        res <- c(res, curr_res)
        curr_res <- NULL
      } else {
        curr_res <- paste0(curr_res, out, collapse= "")
      }
    }
    res <- c(res, curr_res)
  }

  res
}

reference_tag_lvl1 <- function(x) {
  res <- NULL

  if ("tag_href" %in% class(x)) {
    address <- as.character(x[[1]])
    label_list <- map_chr(x[[2]], reference_single_tag)
    label <- paste0(label_list, collapse = " ")
    res <- paste0("[", label, "](", address, ")")
  }

  # Verify if this is still needed
  if ("tag_arguments" %in% class(x)) {
    res <- map(x, ~ map(.x, reference_tag_lvl2))
    #res <- res[res != ""]
  }

  if ("tag_item" %in% class(x)) {
    res <- map(x, ~ map(.x, reference_tag_lvl2))
    #res <- res[res != ""]
  }

  res <- tag_itemize(x, res)

  if (is.null(res)) {
    res_map <- map(x, reference_tag_lvl2)
    if(length(res_map) == 1) {
      res <- res_map[[1]]
    } else {
      curr_res <- NULL
      for(i in seq_along(res_map)) {
        curr <- res_map[[i]]
        if (length(curr) == 1) {
          if(curr == "\n") {
            res <- c(res, curr_res)
            curr_res <- NULL
          } else {
            curr_res <- paste0(curr_res, curr, collapse= "")
          }
        } else {
          res <- c(res, flatten(curr))
        }
      }
    }
    
  }

  res
}

reference_tag_example <- function(x) {
  if (length(x) == 1) {
    res <- reference_single_tag(x)
  } else {
    res <- map_chr(x, reference_single_tag)
    #res <- res[res != ""]
  }
  res
}

reference_tag_lvl2 <- function(x, col = "\n") {
  res <- NULL
  if (length(x) == 1) {
    res <- tag_itemize(x, res)
    if(is.null(res)) res <- reference_single_tag(x, x)
  } else {
    res <- tag_itemize(x, res)
    if (is.null(res)) {
      res <- map(x, reference_single_tag)
      #res <- res[res != ""]
      #res <- paste0(res, collapse = col)
    }
  }
  res
}

tag_itemize <- function(x, res) {
  if ("tag_itemize" %in% class(x) && is.null(res)) {
    curr_res <- NULL
    for (i in seq_along(x)) {
      curr <- x[[i]]
      if (length(curr) == 0) {
        if (curr_res == "") {
          item <- curr
        } else {
          item <- paste0("- ", curr_res)
        }
        res <- c(res, item)
        curr_res <- NULL
      } else {
        curr_res <- trimws(paste0(curr_res, " ", reference_single_tag(curr))) 
      }
    }
    if (curr_res == "") {
      item <- curr
    } else {
      item <- paste0("- ", curr_res)
    }
    res <- c(res, item)
    res <- flatten(res)
  }
  res
}

reference_single_tag <- function(x, label = NULL) {
  if (!is.null(label)) {
    res <- label
  } else {
    res <- as.character(x)
  }
  if (length(res) == 1) {
    if (res != "\n" && res != "") {
      res <- gsub("\n", " ", res)
      res <- ifelse("tag_pkg" %in% class(x), paste0("`", res, "`"), res)
      res <- ifelse("tag_code" %in% class(x), paste0("`", res, "`"), res)
      res <- ifelse("RCODE" %in% class(x) && !is.null(label), paste0("`", res, "`"), res)
    }
  } else {
    res <- ""
  }
  res
}

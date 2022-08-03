
test_function <- function() {
  pkg <- pkgdown::as_pkgdown()
  topics <- transpose(pkg$topics)
  topic <- topics[[7]]
  
  tag_names <- map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)
  
  qmd_template <- path("inst", "templates", "template.qmd")
  
  qmd_lines <- readLines(qmd_template)
  
  is_tag <- substr(names(tags), 1, 4) == "tag_"
  tags <- tags[is_tag]
  
  res <- qmd_lines
  for(i in seq_along(tags)) {
    tag <- substr(names(tags)[i], 5, nchar(names(tags)[i]))
    res <- replace_tag(res, tag, tags[[i]])
  }
  
  args_loc <- grep("\\{argument\\}", res)
  if(length(args_loc) > 0) {
    args_loc <- args_loc[[1]]
    args_all <- reference_arguments(tags$tag_arguments)
    res_top <- res[1:args_loc - 1]
    if(args_loc >= length(res)) {
      res_bottom <- NULL
    } else {
      res_bottom <- res[(args_loc + 1):length(res)]  
    }
    res <- c(res_top, args_all, res_bottom)
  }
  writeLines(res, path("test.qmd"))
  quarto_render(path("test.qmd"))
}


replace_tag <- function(x, tag, value) {
  gsub(paste0("\\{", tag, "\\}"), reference_parse_tag(value), x)
}



check_all_reference <- function(pkg, silent = FALSE) {
  if(is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- pkg$topics
  files_in <- topics$file_in
  out <- list()
  for(i in seq_len(length(files_in))) {
    if(!silent) print(paste(i, " - ", files_in[[i]]))
    tags <- files_in[[i]] %>% 
      reference_to_list_page(pkg)
    
    ref <- list(tags)
    names(ref) <- files_in[[i]]
    
    out <- c(out, ref)
  }
  out
}

check_dont_runs <- function(pkg) {
  pkg %>% 
    check_all_reference(silent = TRUE) %>% 
    map(~ .x$examples$code_dont_run) %>% 
    discard(is.null)
}

check_identified_sections <- function(pkg) {
  
  identified <- c(
    "name", "title", "usage", "description", "arguments", 
    "section","value", "examples", "seealso", "details",
    "format", "alias", "docType", "keyword", "note",
    "concept", "author"
  )
  
  pkg %>% 
    check_all_reference(silent = TRUE) %>%   
    map(
      ~ .x %>% 
        names() %>% 
        map_chr(~ ifelse(.x %in% identified, "X", .x)) %>% 
        discard(~ .x == "X")
    ) %>% 
    reduce(c) %>% 
    unique()
  
}

check_find_section <- function(section, pkg) {
    pkg %>% 
      check_all_reference(silent = TRUE) %>% 
      map(~ .x[[section]]) %>% 
      discard(is.null)
}




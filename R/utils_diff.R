#' @export 
diff_files <- function(quarto_folder) {
  
  e_file <- path(quarto_folder, ".ecodown")
  
  if(!file_exists(e_file)) {
    cat("No .ecodown file found \n")
    return(list("file"))
  }  
  
  ecodown_file <- readLines(e_file)[[1]]
  
  all_commits <- commits()
  all_shas <- map_chr(all_commits, ~.x$sha)
  
  y <- all_commits[all_shas == ecodown_file][[1]]
  x <- last_commit()
  
  all_diffs <- diff(tree(y), tree(x))
  
  all_files <- map_chr(all_diffs$files, ~ .x$new_file)
  
  all_ext <- path_ext(all_files)
  
  full_refresh <- FALSE
  
  quarto_file <- any(path_file(all_files) == "_quarto.yml")
  if(quarto_file) {
    cat("FULL UPDATE TRIGGERED - Quarto file update\n")
  } else {
    cat("Quarto file not updated\n")
  }
  js_ext <- any(all_ext == "js")
  if(js_ext) {
    cat("FULL UPDATE TRIGGERED - JavaScript files updated\n")
  } else {
    cat("No JS file changes\n")
  }
  css_ext <- any(all_ext == "css")
  if(css_ext) {
    cat("FULL UPDATE TRIGGERED - CSS files updated\n")
  } else {
    cat("No CSS file changes\n")
  }
  
  full_refresh <- any(c(quarto_file, js_ext, css_ext))
  
  if(!full_refresh) {
    is_rend <- path_ext(all_files) %in% c("md", "qmd", "Rmd")
    
    rend_files <- all_files[is_rend]
    
    rend_abs <- path_abs(rend_files)
    
    qi <- yaml::read_yaml("_quarto.yml")
    
    side_bar <- qi$website$sidebar
    
    list_href <- function(x) {
      ft <- map(x, ~ {
        if(class(.x) == "list") {
          xc <- .x
          map(xc, ~ {
            xr <- .x$href
            if(is.null(xr)) {
              xt <- .x$contents
              if(is.list(xt)) {
                xr <- map(xt, ~.x$href)
              }
            } 
            xr
          })
        }
      })
      flatten(ft)
    }  
    
    side_bar_parsed <- map(side_bar, ~{
      x1 <- .x
      x1_href <- x1$href
      x2 <- list_href(x1)
      if(!is.null(x1_href)) {
        x3 <- c(x2, x1_href)
      } else {
        x3 <- x2
      }
      flatten(x3)
      
    })
    
    sb_abs <-  map(side_bar_parsed, path_abs)
    
    rend_mtc <- map(
      rend_abs, ~{
        x1 <- .x
        rf <- map_lgl(sb_abs, ~ any(.x == x1))
        if(any(rf)) {
          reduce(sb_abs[rf], function(x, y) c(x,y) )
        } else {
          NULL
        }
      }
    )
    
    if(length(rend_mtc) > 0) {
      rend_red <- reduce(rend_mtc, function(x, y) c(x,y) )  
    } else {
      rend_red <- list()
    }
    
    files_diff <- unique(c(rend_abs, rend_red))
    
    cat(length(files_diff), "renderable files to be updated\n")
    
    files_diff
  } else {
    list("full")
  }
}
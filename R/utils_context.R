ecodown_context <- new.env(parent = emptyenv())

ecodown_context_set <- function(id, vals = list()) {
  ecodown_context[[id]] <- vals
}

ecodown_context_get <- function(id) {
  if (id == "") {
    return(NULL)
  }
  ecodown_context[[id]]
}

get_verbosity <- function() {
  x <- ecodown_context_get("verbosity")
  if (is.null(x)) x <- "verbose"
  x[[1]]
}

get_clone_header <- function() {
  x <- ecodown_context_get("clone_header")
  if (is.null(x)) x <- 0
  x[[1]]
}

set_clone_header <- function(x) {
  ecodown_context_set("clone_header", x)
}

get_package_header <- function() {
  x <- ecodown_context_get("package_header")
  if (is.null(x)) x <- 0
  x[[1]]
}

set_package_header <- function(x) {
  ecodown_context_set("package_header", x)
}

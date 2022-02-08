#' Download the package's latest source code from repo
#' @param repo_url Repo location
#' @param target_folder Location to copy the package to. Defaults to a temporary
#' directory
#' @param branch Repo branch. Defaults to 'main' 
#' @inheritParams ecodown_build
#' @examples 
#' pkg_path <- ecodown_clone("https://github.com/tidyverse/hms")
#' list.files(pkg_path)
#' @export
ecodown_clone <- function(repo_url = "",
                          target_folder = tempdir(),
                          branch = "main",
                          verbosity = c("verbose", "summary", "silent")) {
  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  msg_color_title("Cloning repo")

  if (verbosity == "summary" && get_clone_header() == 0) {
    msg_summary_entry("       Clone / Checkout       \n")
    set_clone_header(1)
  }

  pkg_name <- path_file(repo_url)

  pkg_dir <- path(target_folder, pkg_name)

  msg_color("Cloning:", bold(pkg_name), return = FALSE, color = magenta)

  start_clone <- Sys.time()
  git_clone(url = repo_url, path = pkg_dir, verbose = FALSE, branch = branch)
  
  msg_color_line(cat_time(start_clone, Sys.time()), "\n")
  
  pkg_dir
  
}

checkout_repo <- function(pkg_dir = "",
                          commit,
                          branch,
                          verbosity,
                          pkg_name) {
  
  verbosity <- verbosity[1]
  commit <- commit[1]
  
  ecodown_context_set("verbosity", verbosity)
  
  repo_tags <- git_tag_list(repo = pkg_dir)
  repo_log <- git_log(repo = pkg_dir, ref = branch)
  using_commit <- NULL
  
  if (commit == "latest_tag") {
    tag_logs <- map(
      repo_tags$commit,
      ~ {
        x <- repo_log[repo_log$commit == .x, ]
        if (nrow(x) == 0) x <- NULL
        x
      }
    )
    flat_dates <- as.double(flatten(map(tag_logs, ~ .x$time)))
    tag_dates <- sort(flat_dates, decreasing = TRUE)
    log_match <- map(tag_logs, ~ {
      x <- .x$time == tag_dates[1]
      if (length(x) > 0) {
        if (x) {
          .x$commit
        }
      } else {
        NULL
      }
    })
    log_match <- flatten(log_match)
    if (length(log_match) > 0) {
      using_commit <- log_match[[1]]
      matched_tag <- repo_tags[repo_tags$commit == using_commit, ]
      ck_msg <- matched_tag$name
      ck_type <- "tag"
    }
  }

  if (commit != "latest_tag" && commit != "latest_commit") {
    using_commit <- commit
    ck_msg <- paste0(substr(commit, 1, 7), "...")
    ck_type <- "sha"
  } 
  
  if(is.null(using_commit) | commit == "latest_commit") {
    latest_record <- head(repo_log[repo_log$time == max(repo_log$time), ], 1)
    using_commit <- latest_record$commit
    ck_msg <- "Latest"
    ck_type <- "latest"
  }
  
  sha <- checkout_commit(repo = pkg_dir, 
                  commit = using_commit, 
                  ck_type = ck_type, 
                  tag = ck_msg
                  )
  
  sum_msg <- paste0(pkg_name, " (", ck_msg, ")")
  if (get_package_header() == 0) msg_summary_entry("\n")
  msg_summary_number(sum_msg, size = 30, side = "right")
  sha
  
}

checkout_commit <- function(repo = "", commit = "", ck_type = NULL, tag = NULL) {
  if(ck_type == "tag") {
    msg_color("Checking out tag:", tag, color = magenta)
  } 
  if(ck_type == "latest") {
    msg_color("Using", bold("latest"),"commit", color = magenta)  
  }
  commit <- commit[1]
  msg_commit <- paste0("Checking out SHA: ", substr(commit, 1, 7), "...")
  msg_color(msg_commit, color = magenta)
  msg_summary_entry(msg_commit, color = magenta)
  git_branch_create(substr(commit, 1, 7), ref = commit, repo = repo)
  commit
}

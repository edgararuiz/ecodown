#' Download the package's latest source code from repo
#' @param repo_url Repo location
#' @param commit Commit to use as the base for the documentation.  It defaults
#' to 'latest_tag'. That default will search for the latest Git tag.  The
#' assumption is that the latest tag is the same as the latest release.  This
#' way we avoid documenting work-in-progress.  The 'latest_commit' value will
#' simply use whatever is cloned. Pass an SHA value if you wish to fix the
#' commit to use.
#' @param target_folder Location to copy the package to. Defaults to a temporary
#' directory
#' @param branch Repo branch. Defaults to 'main'
#' @inheritParams ecodown_build
#' @examples 
#' pkg_path <- ecodown_clone("https://github.com/tidyverse/hms")
#' list.files(pkg_path)
#' @export
ecodown_clone <- function(repo_url = "",
                          commit = c("latest_tag", "latest_commit"),
                          target_folder = tempdir(),
                          branch = "main",
                          verbosity = c("verbose", "summary", "silent")) {
  verbosity <- verbosity[1]
  commit <- commit[1]
  
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
  
  checkout_repo(pkg_dir = pkg_dir, 
                commit = commit, 
                branch = branch, 
                verbosity = verbosity,
                pkg_name = pkg_name
                )
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
      msg_color("Checking out tag:", matched_tag$name, color = magenta)
      git_branch_create("currenttag", ref = using_commit, repo = pkg_dir)
      ck_msg <- matched_tag$name
    }
  }
  
  if(is.null(using_commit) | commit == "latest_commit") {
    latest_record <- head(repo_log[repo_log$time == max(repo_log$time), ], 1)
    using_commit <- latest_record$commit
    ck_msg <- "Latest"
    msg_color("Using", bold("latest"),"commit", color = magenta)
    checkout_commit(pkg_dir, using_commit)
  }
  
  if (commit != "latest_commit" && is.null(using_commit)) {
    checkout_commit(pkg_dir, commit)
    ck_msg <- paste0(substr(commit, 1, 7), "...")
  } 
  
  sum_msg <- paste0(pkg_name, " (", ck_msg, ")")
  if (get_package_header() == 0) msg_summary_entry("\n")
  msg_summary_number(sum_msg, size = 30, side = "right")

  pkg_dir
}

checkout_commit <- function(repo = "", commit = "") {
  commit <- commit[1]
  msg_color("Checking out SHA:", substr(commit, 1, 7), "...", color = magenta)
  msg_summary_entry(
    paste0("Checking out SHA: ", substr(commit, 1, 7), "..."), 
    color = magenta
    )
  git_branch_create(substr(commit, 1, 7), ref = commit, repo = repo)
}

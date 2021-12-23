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
#' @param verbosity Level of messaging available during run time. Possible values
#' are 'verbose', 'summary', and 'silent'.  Defaults to: 'verbose'
#' @export
ecodown_clone <- function(repo_url = "",
                          commit = c("latest_tag", "latest_commit"),
                          target_folder = tempdir(),
                          branch = "main") {
  msg_color_title("Cloning repo")

  if (verbosity == "summary" && get_clone_header() == 0) {
    msg_summary_entry("       Clone / Checkout       ")
    set_clone_header(1)
  }

  pkg_name <- path_file(repo_url)

  pkg_dir <- path(target_folder, pkg_name)

  msg_color("Cloning: ", pkg_name, color = green)

  git_clone(url = repo_url, path = pkg_dir, verbose = FALSE)

  if (commit[1] == "latest_tag") {
    repo_tags <- git_tag_list(repo = pkg_dir)
    repo_log <- git_log(repo = pkg_dir)
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
    matched_tag <- repo_tags[repo_tags$commit == log_match[[1]], ]
    msg_color("Checking out tag: ", matched_tag$name, color = green)
    git_branch_create("currenttag", ref = log_match, repo = pkg_dir)
    ck_msg <- matched_tag$name
  } else {
    if (commit[1] != "latest_commit") {
      checkout_commit(pkg_dir, commit[1])
      ck_msg <- paste0(substr(commit[1], 1, 7), "...")
    } else {
      ck_msg <- "Latest"
    }
  }


  sum_msg <- paste0(pkg_name, " (", ck_msg, ")")
  msg_summary_number(sum_msg, size = 30, side = "right")

  pkg_dir
}

checkout_commit <- function(repo = "", commit = "") {
  msg_color("Checking out SHA: ", substr(commit[1], 1, 7), "...", color = green)
  git_branch_create("specificsha", ref = commit[1], repo = repo)
}

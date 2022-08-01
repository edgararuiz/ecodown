#' Prepare full Quarto site
#' @param quarto_folder Base target Quarto folder. Defaults to current workspace.
#' @param verbosity Level of messaging available during run time. Possible values
#' are 'verbose', 'summary', and 'silent'.
#' @export
ecodown_build <- function(quarto_folder = here::here(),
                          verbosity = c("summary", "verbose", "silent")) {
  verbosity <- verbosity[1]

  ecodown_context_set("verbosity", verbosity)

  qbf <- quarto_folder

  config_path <- path(qbf, "_ecodown.yml")

  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)

    cat(bold(black(">> Cloning and Converting packages from repos\n\n")))


    pkgs <- config_yaml$site$clone_convert$packages
    if (!is.null(pkgs)) {
      cc_args <- config_yaml$site$clone_convert
      cc_args <- cc_args[names(cc_args) != "packages"]
    } else {
      pkgs <- config_yaml$site$packages
      cc_args <- NULL
    }


    walk(
      pkgs, ~ {
        all_args <- c(cc_args, .x)
        exec(
          "ecodown_clone_convert",
          !!!all_args,
          quarto_folder = qbf,
          verbosity = verbosity
        )
      }
    )

    if (null_true(config_yaml$site$quarto$run)) {
      run_autolink <- null_true(config_yaml$site$autolink$run)
      msg_quarto <- "\n>> Rendering to HTML"
      if (run_autolink) msg_quarto <- paste0(msg_quarto, " and auto-linking")
      cat(bold(black(paste0(msg_quarto, "\n"))))
      exec_command(
        "ecodown_quarto_render",
        config_yaml$site$quarto,
        addl_entries = list(
          quarto_folder = qbf,
          verbosity = verbosity,
          autolink = run_autolink
        )
      )
    }

    if (null_true(config_yaml$site$autolink$run)) {
      ecodown_autolink(quarto_folder = quarto_folder, verbosity = verbosity)
    }
  }
  lc <- last_commit()
  lc_sha <- lc$sha
  writeLines(lc_sha, con = path(quarto_folder, ".ecodown"))
}

null_true <- function(x) {
  if (is.null(x)) {
    res <- TRUE
  } else {
    res <- x
  }
  res
}

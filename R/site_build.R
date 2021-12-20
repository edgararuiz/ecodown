#' Prepare full Quarto site
#' @inheritParams package_clone_and_build
#' @export
site_build_quarto <- function(quarto_base_folder = here::here()) {
  qbf <- quarto_base_folder
  config_path <- path(qbf, "_ecodown.yml")
  if (file_exists(config_path)) {
    config_yaml <- read_yaml(config_path)

    walk(
      config_yaml$site$packages, ~ {
        exec(
          "package_clone_and_build",
          !!!.x,
          quarto_base_folder = qbf
        )
      }
    )

    qr <- config_yaml$site$quarto_render
    if (!is.null(qr)) {
      exec("quarto_render", !!!qr, input = qbf)
    } else {
      quarto_render(input = qbf, as_job = FALSE)
    }

    al <- config_yaml$site$autolink
    if (!is.null(al)) {
      exec("site_autolink_html", !!!al)
    } else {
      site_autolink_html(quarto_base_folder = quarto_base_folder)
    }
  }
}

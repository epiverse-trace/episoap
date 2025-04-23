create_config <- function(path = file.path(tempdir(), "config.yaml")){

  if (!file.exists(path)) {
    config_template <- list(
      severity = list(
        data = NA_character_,
        disease_name = NA_character_,
        total_cases = NA_real_,
        total_deaths = NA_real_,
        death_in_confirmed = NA_real_,
        account_for_delay = TRUE,
        epidist_params = list(
          type = NA_character_,
          distribution = NA_character_,
        parameters = list(
          meanlog = NA_real_,
          sdlog = NA_real_,
          shape = NA_real_,
          scale = NA_real_
        )

      ),
        interval = NA_character_

      )

    )

    yaml::write_yaml(config_template, path)

  }

  if (Sys.info()["sysname"] == "Darwin") {
    system(paste("open", shQuote(path)), wait = FALSE)
  } else if (Sys.info()["sysname"] == "Windows") {
    shell.exec(path)
  } else {
    system(paste("xdg-open", shQuote(path)), wait = FALSE)
  }

  invisible(path)
}


#' Create and Open a Configuration YAML File Template
#'
#' This function generates a configuration file in a YAML format with a
#' structured template for epidemiological severity parameters. If the file does
#' not exist at the specified path, it creates one with placeholder values. The
#' file is then opened in the system's default application for YAML/text files.
#'
#' @param path A character with the file path where the configuration file
#'    should be created/opened. When not provided, default to a file named
#'    `config.yaml` in the system's temporary directory `tempdir()`.
#'
#' @returns Invisibly returns the `path` to the created or existing
#'    configuration file.
#'
#' @examples
#' # Create/open config in temp directory
#' create_config()
#'
#' # Create/open in working directory
#' create_config("my_config.yaml")
#'
#' @note
#' - Replace `NA` with appropriate values before using the configuration.
#' - The function does not validate YAML content.
#'
#' @export
create_config <- function(path = file.path(tempdir(), "config.yaml")) {

  # create the config file template
  if (!file.exists(path)) {
    config_template <- list(
      data = NA_character_,
      disease_name = NA_character_,
      severity = list(
        total_cases = NA_real_,
        total_deaths = NA_real_,
        death_in_confirmed = NA_real_,
        interval = NA_character_,
        epidist = NULL,
        epidist_params = list(
          type = NA_character_,
          distribution = NA_character_,
        parameters = list(
          meanlog = NA_real_,
          sdlog = NA_real_,
          shape = NA_real_,
          scale = NA_real_
        )
        )
      )

    )

    yaml::write_yaml(config_template, path)

  }

  # Open the config file template
  system_info <- Sys.info()["sysname"]
  switch(
    system_info,
    "Darwin" = system(paste("open", shQuote(path)), wait = FALSE),
    "Windows" = shell.exec(path),
    "Linux" = system(paste("xdg-open", shQuote(path)), wait = FALSE)
  )

  # if (Sys.info()["sysname"] == "Darwin") {
  #   system(paste("open", shQuote(path)), wait = FALSE)
  # } else if (Sys.info()["sysname"] == "Windows") {
  #   shell.exec(path)
  # } else {
  #   system(paste("xdg-open", shQuote(path)), wait = FALSE)
  # }

  invisible(path)
}


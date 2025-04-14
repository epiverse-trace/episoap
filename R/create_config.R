create_config <- function(path = "config.yaml"){

  if (file.exists(path)) {
    config_template <- list(
      severity = list(

        data = NA_character_,
        disease_name = NA_character_,
        total_cases = NA_real_,
        total_deaths = NA_real_,
        death_in_confirmed = NA_real_,
        account_for_delay = TRUE,
        epidist = list(
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

  }

  #YAML structure with placeholders
  yaml_template <- c(
    "# Epidemiological Pipeline Configuration",
    "# --------------------------------------",
    "# Replace null values with appropriate parameters",
    "# Required parameters marked with [REQUIRED]",
    "",
    "severity:",
    "  data: null              # [REQUIRED] Path to input data file",
    "  disease_name: null      # [REQUIRED] Name of disease",
    "  total_cases: null       # [REQUIRED] Total confirmed cases (numeric)",
    "  total_deaths: null      # [REQUIRED] Total confirmed deaths (numeric)",
    "  death_in_confirmed: null # Probability of death in confirmed cases (0-1)",
    "  account_for_delay: TRUE # TRUE/FALSE for delay correction",
    "  epidist:",
    "    type: null            # Distribution type (e.g., 'incubation')",
    "    distribution: null    # Statistical distribution (e.g., 'gamma')",
    "    parameters:",
    "      meanlog: null       # Log-mean for lognormal distributions",
    "      sdlog: null         # Log-sd for lognormal distributions",
    "      shape: null         # Shape parameter for weibull/gamma",
    "      scale: null         # Scale parameter for weibull/gamma",
    "  interval: null          # Time interval for estimates",
    ""
  )
#tryCatch(
  {
    writeLines(yaml_template, path)
    message("Configuration template created at:\n", normalizePath(path))
  },
  error = function(e) {
    stop("Failed to create config file:\n", e$message)
  }
  )

# interactive session to open file
if (isTRUE(open_file)) {
  choice <- menu(
    title = "\nWould you like to edit the config file now?",
    choices = c("Yes - Open in default editor", "No - I'll edit it later")
  )

  if (choice == 1) {
    tryCatch(
      {
        utils::file.edit(path)
        message("Close the editor when finished...")
      },
      error = function(e) {
        message("Could not open editor automatically. ",
                "Please edit the file manually at:\n", normalizePath(path))
      }
    )
  }
}

  invisible(path)
}





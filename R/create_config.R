create_config <- function(path = "config.yaml", open_file = interactive()){

  if (file.exists(path)) {
    message("Configuration file already exists at:\n", normalizePath(path))
    return(invisible(path))
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





create_config <- function(path = "config.yaml"){

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
    "  account_for_delay: null # TRUE/FALSE for delay correction",
    "  epidist:",
    "    type: null            # Distribution type (e.g., 'incubation')",
    "    distribution: null    # Statistical distribution (e.g., 'gamma')",
    "    parameters:",
    "      meanlog: null       # Log-mean for lognormal distributions",
    "      sdlog: null         # Log-sd for lognormal distributions",
    "      shape: null         # Shape parameter for weibull/gamma",
    "      scale: null         # Scale parameter for weibull/gamma",
    "  interval: null          # Time interval for estimates",
    "",
  )

}

#' Load Configuration Parameters
#'
#' @param path Path to config file (default: tempdir()/config.yaml)
#' @return List with loaded data and parameters
#' @export
load_config <- function(path = file.path(tempdir(), "config.yaml")) {
  config <- yaml::read_yaml(path)

  # Initialize(empty to store) data and counts
  data <- NULL
  total_count <- NULL
  total_death <- NULL

  # Load data if path provided
  if (!is.na(config$data)) {
    if (!file.exists(config$data)) {
      stop("Data file not found: ", config$data)
    }

    ext <- tools::file_ext(config$data)
    data <- switch(tolower(ext),
                   csv = read.csv(config$data),
                   rds = readRDS(config$data),
                   stop("Unsupported file format: ", ext)
    )
  }

  # Get counts from severity section
  severity <- config$severity
  total_count <- severity$total_cases %||% NULL  # Handle YAML null
  total_death <- severity$total_deaths %||% NULL

  # Validate mutual exclusivity
  if (!is.null(data) && (!is.null(total_count) || !is.null(total_death))) {
    stop("Configuration conflict: Provide either data OR counts, not both")
  }

  if (is.null(data) && (is.null(total_count) || is.null(total_death))) {
    stop("Must provide either data file or both total_cases/total_deaths")
  }

  list(
    data = data,
    total_count = total_count,
    total_death = total_death
  )
}

# Helper for NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a

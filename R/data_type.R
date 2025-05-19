#' Get Data Type from YAML Configuration
#'
#' @param path Path to config file (default: tempdir()/config.yaml)
#' @return Character string indicating data type
#' @export
data_type <- function(path = file.path(tempdir(), "config.yaml")) {
  config <- load_config(path)
  get_data_type(
    data = config$data,
    total_count = config$total_count,
    total_death = config$total_death
  )
}

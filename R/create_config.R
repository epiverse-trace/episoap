create_config <- function(path = "config.yaml"){

  if (file.exists(path)) {
    message("Configuration file already exists at:\n", normalizePath(path))
    return(invisible(path))
  }
}

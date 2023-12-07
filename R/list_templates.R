#' List available rmarkdown templates
#'
#' @returns A character vector containing the list of rmarkdown included in the
#' package
#'
#' @examples
#' list_templates()
#'
#' @export
#'
list_templates <- function() {

  reps <- list.files(system.file("rmarkdown", "templates", package = "episoap"))
  stable_reps <- grep("^_", reps, invert = TRUE, value = TRUE)

  return(stable_reps)

}

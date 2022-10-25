#' List  available rmarkdown templates
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

  list.files(system.file("rmarkdown", "templates", package = "soap"))

}

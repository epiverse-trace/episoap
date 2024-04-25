
code_entry <- "\n```{r echo=TRUE, eval=TRUE}"
not_eval_code_entry <- "\n```{r echo=TRUE, eval=FALSE}"
code_end <- "```"

create_template <- function() {
  withr::with_tempdir(
    {
      file.copy(
        from      = system.file("rmarkdown", "templates", "template.Rmd",
                                package  = "episoap",
                                mustWork = TRUE),
        to        = getwd(),
        overwrite = TRUE
      )
      template_path  <- file.path(getwd(), "template.Rmd")

    },
    tmpdir  = tempdir(),
    clean   = FALSE, # when TRUE, the folder is deleted and report is not
    # printed out
    fileext = "",
    pattern = "template_"
  )

  return(template_path)
}

#' Add text to a template Rmd file
#'
#' @param template_path path to the template file
#' @param text the text to add to the template file
#'
#' @return The input file will be updated with the new text if it is
#'    successfully added
#' @keywords internal
#'
#' @examples
#' template_path <- create_template()
#' text <- "## Import the data"
add_text_to_template <- function(template_path, text = "") {
  write("\n\n", file = template_path, append = TRUE)
  write(text, file = template_path, append = TRUE)
}

#' Add text to a template Rmd file
#'
#' @param template_path path to file
#' @param code the R code to add to the template
#'
#' @return if success, add the provided code chunk into the template
#' @keywords internal
#'
#' @examples
#' # add this code when the input is file path
#' code <- "rio::import(system.file('extdata', 'covid_hosp_uk_20201024.xlsx',
#'                                  package = 'episoap'))"
#'
add_code_to_template <- function(template_path, code) {
  write(code_entry, file = template_path, append = TRUE)
  write(code, file = template_path, append = TRUE)
  write(code_end, file = template_path, append = TRUE)
}


# the challenge is to:
# 1. define the algorithm for a given workflow
# 2. find a way to encapsulate code as a string into an object. 'quote' works
#    but adds '{' at the beginning and '}' at the end.
# 3.

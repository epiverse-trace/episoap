#' Create report from template
#'
#' @param report A character vector of report template(s) to render (default to all
#'   templates)
#' @param out_dir A character vector with the output directory (default to an
#'   `episoap_report` in the current directory
#' @param preview A logical (default `TRUE`) indicating whether the rendered
#'   report should be opened in the default browser
#' @param ... Arguments passed as parameters to the report template(s)
#'
#' @returns (invisibly) a character vector of paths to the rendered reports
#'
#' @export
#'
#' @examples
#' # Download data we need for this example
#' wd <- file.path(tempdir(), "episoap_report")
#' dir.create(wd)
#' download.file(
#'   "https://github.com/epiverse-trace/episoap/blob/main/inst/rmarkdown/templates/transmissibility/skeleton/data/covid_linelist_england.rds?raw=true",
#'   file.path(wd, "covid_linelist_england.rds")
#' )
#' # Running the pipeline with custom data saved on your computer
#' run_pipeline(
#'   report = "transmissibility",
#'   out_dir = wd,
#'   data_file = "covid_linelist_england.rds"
#' )
run_pipeline <- function(
    report = list_templates(),
    out_dir = "episoap_report",
    preview = TRUE,
    ...
) {

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  withr::local_dir(out_dir)

  report <- match.arg(report)

  rendered <- vapply(report, function(r) {
    rmarkdown::draft(
      file = paste0(r, ".Rmd"),
      template = r,
      create_dir = FALSE,
      package = "episoap",
      edit = FALSE
    )
    rmarkdown::render(
      paste0(r, ".Rmd"),
      params = list(...)
    )
  }, character(1))

  if (preview) {
    utils::browseURL(rendered[[1]])
  }

  return(invisible(rendered))
}

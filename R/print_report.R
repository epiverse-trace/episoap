
#' @importFrom utils browseURL
print_report <- function(pipeline_report,
                         report_title     = "{episoap} modular pipeline",
                         output_file_name = NULL,
                         format           = "html",
                         print            = TRUE) {

  checkmate::assert_list(pipeline_report, min.len = 1L, null.ok = FALSE)
  checkmate::check_names(
    pipeline_report,
    subset.of = c("res_transmissibility", "res_severity")
  )


  # generate output file and directory
  timestamp_string   <- format(Sys.time(), "_%Y-%m-%d%_at_%H%M%S")
  if (is.null(output_file_name)) {
    output_file_name <- paste0("episoap_", timestamp_string)
  }

  # this ensures to add the logo to the report
  pipeline_report[["report_title"]] <- report_title
  man_path                          <- file.path("man", "figures")
  pipeline_report[["logo"]]         <- system.file(man_path, "logo.svg",
                                                   package = "episoap")

  # Temporarily copy Rmd file from package library into save_directory so that
  # intermediate files also get created there.
  # NOTE: explicitly setting intermediates_dir in rmarkdown::render() to
  # save_directory or tempdir() causes duplicate chunk label errors when package
  # is subjected to the github actions on GitHub.
  withr::with_tempdir(
    {
      file.copy(
        from      = system.file("rmarkdown", "templates", "printing-rmd",
                                "skeleton", "skeleton.Rmd",
                                package  = "episoap",
                                mustWork = TRUE),
        to        = getwd(),
        overwrite = TRUE
      )

      file_and_path  <- file.path(getwd(), paste0(output_file_name, ".html"))
      stopifnot("Invalid format: Only 'html' format is currently supported." =
                  format == "html")
      message("Generating html report in ", getwd())
      rmarkdown::render(
        input       = file.path(getwd(), "skeleton.Rmd"),
        output_file = file_and_path,
        output_dir  = getwd(),
        params      = pipeline_report,
        quiet       = TRUE
      )
    },
    tmpdir  = tempdir(),
    clean   = FALSE, # when TRUE, the folder is deleted and report is not
    # printed out
    fileext = "",
    pattern = "cleanepi_report_"
  )

  # print report if specified
  if (print) {
    utils::browseURL(file_and_path)
  }
  return(file_and_path)
}

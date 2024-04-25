
#' Title
#'
#' @param data
#' @param disease
#' @param transmissibility
#' @param severity
#'
#' @return
#' @export
#'
#' @examples
make_template <- function(data,
                          disease = "COVID",
                          transmissibility = TRUE,
                          severity = TRUE) {
  # copy the Rmd template in the package into a temp dir
  template_path <- create_template()

  # if data is file path, add a chunck to import it
  add_data_import_chunck(template_path, data)

  # if transmissibility = TRUE, add the chunk for transmissibility
  if (transmissibility) {
    add_transmissibility(template_path = template_path,
                         data          = data,
                         disease       = disease)
  }
}

add_data_import_chunck <- function(template_path, data) {
  if (is.character(data) && file.exists(data)) {
    add_text_to_template(template_path, text = "## Import the data")
    code <- "rio::import(data)"
    add_code_to_template(template_path, code)
  }
}

add_transmissibility <- function(template_path, data, disease = disease) {
  # make sure that the date values are of class Date
  add_text_to_template(
    template_path = template_path,
    text = "## make sure that the date values are of class Date"
  )
  code <- quote(
    {
      data <- data |>
        dplyr::mutate(dplyr::across(dplyr::where(\(x) inherits(x, "POSIXct")),
                                    as.Date))
    }
  )
  # remove '{' at the start and '}' at the end of the code
  curly_brakets_idx <- which(code == "{" | code == "}")
  code <- as.character(code)[-curly_brakets_idx[1L]]
  add_code_to_template(template_path, code)

  # generate descriptive stats
  add_text_to_template(
    template_path = template_path,
    text = "## Descriptive statistics\n"
  )

  # some explanation
  add_text_to_template(
    template_path = template_path,
    text = "The code below allows to visualize both the daily and weekly
    incidence.
    Make sure to update the date, group, and count variable names if they differ
    from the names used below.\n"
  )
  code <- quote(
    {
      date_var  <- "date"
      group_var <- "region"
      count_var <- "n"

      ## plot both daily and weekly incidences
      if (!inherits(data, "linelist")) {
        ## convert the input data into linelist
        linelist_data <- data %>%
          linelist::make_linelist(date_admission = date_var,
                                  location       = group_var,
                                  counts         = count_var,
                                  allow_extra    = TRUE)
        plot_cases_per_group(linelist_data) # this needs to be replaced by the actual code

        ## convert incidence into weekly incidence using incidence2
        converted_data <- data %>%
          incidence2::incidence(date_index = "date",
                                interval   = "week",
                                counts     = count_var,
                                groups     = group_var)

        plot_weekly_incidence(converted_data) ## this needs to be replaced by the actual code
      }
    }
  )
  curly_brakets_idx <- which(code == "{" | code == "}")
  code <- as.character(code)[-curly_brakets_idx[1L]]
  add_code_to_template(template_path, code)

}


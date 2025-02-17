#' Convert linelist data in the form of an incidence data required by the {cfr}
#' package used to estimate CFR.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>
#' @param date_var_name A \code{<character>} with the name of the column
#'    containing the dates when cases were registered
#' @param cases_status_var_name A \code{<character>} with the name of the column
#'    that contains the information about whether a case was dead or
#'    had recovered
#' @param death_outcome A \code{<character>} with the value, in the
#'    `cases_status_var_name` column, that is used to specify whether the case
#'    was dead
#' @param diagnosis_status_var_name A \code{<character>} with the name of the
#'    column that has the information about whether a case is confirmed,
#'    suspected, etc
#' @param diagnosis_outcome A \code{<character>} with the value, in the
#'    `diagnosis_status_var_name` column, that is used to specify if a case
#'    was confirmed
#'
#' @return A \code{<data.frame>} with the three columns required to estimate
#'    CFR. When the values for `diagnosis_status_var_name` and
#'    `diagnosis_outcome` are not `NULL`, this data frame will have an
#'    attribute, `confirmed_cases_data`, that contains the data to be used to
#'    estimate CFR within confirmed cases only.
#' @export
#'
#' @examples
#' data <- read.csv(system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                              package = "episoap"))
#' cfr_data <- convert_to_incidence(
#'   data = data,
#'   date_var_name = "Onset_week",
#'   cases_status_var_name = "Status",
#'   death_outcome = "dead",
#'   diagnosis_status_var_name = "Type",
#'   diagnosis_outcome = "confirmed"
#' )
convert_to_incidence <- function(data, date_var_name,
                                 cases_status_var_name, death_outcome,
                                 diagnosis_status_var_name = NULL,
                                 diagnosis_outcome = NULL) {

  # convert the linelist into incidence data for all cases
  data[[date_var_name]] <- as.factor(data[[date_var_name]])
  incidence_data <- data %>%
    dplyr::group_by_at(date_var_name) %>%
    dplyr::summarise(
      cases = dplyr::n(),
      deaths = sum(.data[[cases_status_var_name]] == death_outcome,
                   na.rm = TRUE)
    )
  names(incidence_data)[[1L]] <- "date"
  incidence_data[["date"]] <- as.Date(as.character(incidence_data[["date"]]))

  # sometime the date cases was recorded can be provided in weeks or other unit
  # we convert the incidence into daily incidence data
  if (!(length(unique(diff(incidence_data[["date"]]))) == 1L)) {
    incidence_data <- get_sequential_dates(incidence_data)
  }

  # get incidence data for only confirmed cases
  if (!is.null(diagnosis_status_var_name) && !is.null(diagnosis_outcome)) {
    confirmed_cases_incidence_data <- data %>%
      dplyr::group_by_at(date_var_name) %>%
      dplyr::summarise(
        cases = dplyr::n(),
        deaths = sum(.data[[diagnosis_status_var_name]] == diagnosis_outcome &
                       .data[[cases_status_var_name]] == death_outcome,
                     na.rm = TRUE)
      )
    names(confirmed_cases_incidence_data)[[1L]] <- "date"
    confirmed_cases_incidence_data[["date"]] <- as.Date(
      as.character(confirmed_cases_incidence_data[["date"]])
    )
    # convert into daily incidence data
    if (!(length(unique(diff(confirmed_cases_incidence_data[["date"]]))) == 1L)) {
      confirmed_cases_incidence_data <- get_sequential_dates(
        confirmed_cases_incidence_data
      )
    }

    # add the confirmed cases incidence data as an attribute to the incidence
    # data for all cases
    attr(incidence_data, "confirmed_cases_data") <-
      confirmed_cases_incidence_data
  }

  return(incidence_data)
}

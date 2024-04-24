#' Get the input data for {cfr} functions
#'
#' @param data the input data
#' @param date_variable_name the name of the column with the dates when cases
#'    were registered
#' @param cases_status the name of the column with the information about whether
#'    a case was dead or recovered
#' @param death_outcome a character with the value, in the cases_status column,
#'    that is used to specify whether the case was dead
#' @param diagnosis_status the name of the column with the cases diagnosis
#'    outcome.
#' @param diagnosis_outcome the value, in the column with the cases diagnosis
#'    outcome, used to represent the confirmed cases.
#'
#' @return a `list` of 2 elements of type data frame. These are the aggregated
#'    data to calculate CFR across all cases and the one for CFR among the
#'    confirmed cases only. Note that the later will be `NULL` if the
#'    `diagnosis_status` and `diagnosis_outcome` are not provided.
#' @keywords internal
#'
#' @examples
#' data = read.csv(system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                    package = "episoap"))
#' cfr_data <- convert_to_incidence(
#'   data               = data,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed"
#' )
convert_to_incidence <- function(data, date_variable_name,
                                 cases_status, death_outcome,
                                 diagnosis_status  = NULL,
                                 diagnosis_outcome = NULL) {

  # get incidence data for all cases
  data[[date_variable_name]]  <- as.factor(data[[date_variable_name]])
  all_cases_data              <- data %>%
    dplyr::group_by_at(date_variable_name) %>%
    dplyr::summarise(cases  = dplyr::n(),
                     deaths = sum(.data[[cases_status]] == death_outcome,
                                  na.rm = TRUE))
  names(all_cases_data)[[1L]] <- "date"
  all_cases_data[["date"]]    <- as.Date(as.character(all_cases_data[["date"]]))
  # convert into daily incidence data
  if (!(unique(diff(all_cases_data[["date"]])) == 1L)) {
    all_cases_data            <- get_sequential_dates(all_cases_data)
  }

  # get incidence data for only confirmed cases
  confirmed_cases_data <- NULL
  if (!is.null(diagnosis_status) && !is.null(diagnosis_outcome)) {
    confirmed_cases_data      <- data %>%
      dplyr::group_by_at(date_variable_name) %>%
      dplyr::summarise(cases  = dplyr::n(),
                       deaths = sum(.data[[diagnosis_status]] == diagnosis_outcome & # nolint: line_length_linter
                                      .data[[cases_status]] == death_outcome,
                                    na.rm = TRUE))
    names(confirmed_cases_data)[[1L]] <- "date"
    confirmed_cases_data[["date"]]    <- as.Date(as.character(confirmed_cases_data[["date"]])) # nolint: line_length_linter
    # convert into daily incidence data
    if (!(unique(diff(confirmed_cases_data[["date"]])) == 1L)) {
      confirmed_cases_data         <- get_sequential_dates(confirmed_cases_data)
    }
  }

  return(list(
    all_cases_data       = all_cases_data,
    confirmed_cases_data = confirmed_cases_data
  ))
}

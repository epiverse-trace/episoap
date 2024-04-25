#' Execute the user-defined tasks
#'
#' @param disease_name A string with the disease name
#' @param data A data frame or linelist or incidence object
#' @param to_incidence_params A list of parameters needed to convert the input
#'    data into an incidence object. This arguments is only needed when the user
#'    wants to include severity into the run and the data is a linelist or data
#'    frame.
#' @param severity_params A list with the parameters required calculate the
#'    disease severity.
#' @param transmissibility_params A list with the parameters needed to estimate
#'    the disease transmissibility
#'
#' @return Generates a HTML file with the result from each element of the
#'    pipeline
#' @export
#'
#' @examples
#' data <- outbreaks::ebola_kikwit_1995 |>
#'   dplyr::rename("cases" = "onset", "deaths" = "death")
#' disease_name <- "ebola"
#'
#' # define the transmissibility arguments
#' define the arguments for transmissibility
#' transmissibility_params <- list(
#'   date_var = "date",
#'   group_var  = NULL,
#'   count_var = "cases",
#'   incomplete_days = 7,
#'   use_epiparameter_database = TRUE,
#'   si_mean       = NULL,
#'   si_sd         = NULL,
#'   si_dist       = NULL
#' )
#'
#' # define the severity arguments
#' to_incidence_params <- NULL
#' severity_params <- list(
#'   account_for_delay  = FALSE,
#'   epidist            = NULL,
#'   type               = "range",
#'   values             = c(8, 2, 16),
#'   distribution       = "gamma",
#'   interval           = 1,
#'   meanlog            = NULL,
#'   sdlog              = NULL,
#'   total_cases        = NULL,
#'   total_deaths       = NULL,
#'   death_in_confirmed = NULL
#' )
#'
#' # run the pipeline
#' run_pipeline(
#'   disease_name = disease_name,
#'   data = data,
#'   to_incidence_params = to_incidence_params,
#'   severity_params = severity_params,
#'   transmissibility_params = transmissibility_params
#' )
run_pipeline <- function(disease_name,
                         data,
                         to_incidence_params     = NULL,
                         severity_params         = NULL,
                         transmissibility_params = NULL) {

  # define the list of the final results
  final_results <- list()

  # Run the severity pipeline if needed
  if (!is.null(severity_params)) {
    if (!is.null(to_incidence_params)) {
      # convert to incidence data when needed
      data <- convert_to_incidence(
        data               = data,
        date_variable_name = to_incidence_params[["date_variable_name"]],
        cases_status       = to_incidence_params[["cases_status"]],
        death_outcome      = to_incidence_params[["death_outcome"]],
        diagnosis_status   = to_incidence_params[["diagnosis_status"]],
        diagnosis_outcome  = to_incidence_params[["diagnosis_outcome"]]
      )
    }
    res_severity <- run_severity(
      disease_name       = disease_name,
      data               = data,
      account_for_delay  = severity_params[["account_for_delay"]],
      epidist            = severity_params[["epidist"]],
      type               = severity_params[["type"]],
      values             = severity_params[["values"]],
      distribution       = severity_params[["distribution"]],
      interval           = severity_params[["interval"]],
      meanlog            = severity_params[["meanlog"]],
      sdlog              = severity_params[["sdlog"]],
      total_cases        = severity_params[["total_cases"]],
      total_deaths       = severity_params[["total_deaths"]],
      death_in_confirmed = severity_params[["death_in_confirmed"]])

    # add results to the final output
    final_results[["cfr"]] <- res_severity[["cfr"]]
  }

  # Run the transmissibility pipeline if needed
  if (!is.null(transmissibility_params)) {
    res_Rt <- run_transmissibility(
      data = data,
      disease_name              = disease_name,
      date_var                  = transmissibility_params[["date_var"]],
      count_var                 = transmissibility_params[["count_var"]],
      group_var                 = transmissibility_params[["group_var"]],
      incomplete_days           = transmissibility_params[["incomplete_days"]],
      use_epiparameter_database = transmissibility_params[["use_epiparameter_database"]],
      si_mean                   = transmissibility_params[["si_mean"]],
      si_sd                     = transmissibility_params[["si_sd"]],
      si_dist                   = transmissibility_params[["si_dist"]]
    )

    # add results to the final output
    final_results <- c(final_results, res_Rt)
  }

  # print out the report
  print_report(final_results,
               report_title     = "{episoap} modular pipeline",
               output_file_name = NULL,
               format           = "html",
               print            = TRUE)
}

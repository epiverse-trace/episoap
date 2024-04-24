
severity_params <- list(account_for_delay  = FALSE,
                        epidist            = NULL,
                        type               = NULL,
                        values             = NULL,
                        distribution       = NULL,
                        interval           = NULL,
                        meanlog            = NULL,
                        sdlog              = NULL,
                        total_cases        = NULL,
                        total_deaths       = NULL,
                        death_in_confirmed = NULL)

transmissibility_params <- list(use_epiparameter_database = TRUE,
                                group_by      = NULL,
                                interval      = "day",
                                si            = NULL,
                                si_mean       = NULL,
                                si_sd         = NULL,
                                si_dist       = NULL)

to_incidence_params <- list(date_variable_name = NULL,
                            cases_status       = NULL,
                            death_outcome      = NULL,
                            diagnosis_status   = NULL,
                            diagnosis_outcome  = NULL)

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
    final_results <- c(final_results, res_severity)
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

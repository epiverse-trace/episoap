
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

  # convert to incidence data when needed
  # WARNING!!!!! This chunk should go under severity
  if (!is.null(to_incidence_params)) {
    data <- convert_to_incidence(
      data,
      date_variable_name = to_incidence_params[["date_variable_name"]],
      cases_status       = to_incidence_params[["cases_status"]],
      death_outcome      = to_incidence_params[["death_outcome"]],
      diagnosis_status   = to_incidence_params[["diagnosis_status"]],
      diagnosis_outcome  = to_incidence_params[["diagnosis_outcome"]])
  }

  # establish the parameters for the pipeline
  parameters <- list(
    DISEASE_NAME = disease_name,
    DATA         = data
  )

  # --- set parameters for severity
  parameters <- get_severity_params(parameters, severity_params)

  # --- set parameters for transmissibility
  parameters <- get_transmissibility_params(parameters, transmissibility_params)

  # estimate CFR
  tmp_output <- "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/inst/rmarkdown/templates/test.html"
  tmp_input  <- "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/inst/rmarkdown/templates/test_severity.Rmd"
  # tmp_input = file.path(.libPaths(), "episoap",
  #                       "rmarkdown", "templates",
  #                       "test_severity.Rmd")
  rmarkdown::render(input         = tmp_input,
                    params        = parameters,
                    output_dir    = getwd(),
                    output_file   = tmp_output,
                    output_format = NULL)
}



# epidemic_phase = NULL,
# infection_type = "close_contact",
# scope          = NULL,
# study_question = "severity",


# run_pipeline(
#   disease_name      = "Marburg Virus Disease",
#   data              = readRDS(system.file("extdata",
#                                           "Marburg_EqGuinea_incidence.RDS",
#                                           package = "episoap")),
#   account_for_delay = FALSE,
#   epidist           = NULL
# )
#
# run_pipeline(
#   disease_name       = "Marburg Virus Disease",
#   data               = read.csv(system.file("extdata",
#                                             "Marburg_EqGuinea_linelist.csv",
#                                             package = "episoap")),
#   account_for_delay  = TRUE,
#   epidist            = NULL,
#   date_variable_name = "Onset_week",
#   cases_status       = "Status",
#   death_outcome      = "dead",
#   diagnosis_status   = "Type",
#   diagnosis_outcome  = "confirmed",
#   distribution       = "gamma",
#   type               = "range",
#   values             = c(8, 2, 16),
#   interval           = 1
# )
#
# run_pipeline(
#   disease_name       = "Marburg Virus Disease",
#   data               = read.csv(system.file("extdata",
#                                             "Marburg_EqGuinea_linelist.csv",
#                                             package = "episoap")),
#   account_for_delay  = TRUE,
#   epidist            = NULL,
#   date_variable_name = "Onset_week",
#   cases_status       = "Status",
#   death_outcome      = "dead",
#   diagnosis_status   = "Type",
#   diagnosis_outcome  = "confirmed",
#   distribution       = "lnorm",
#   meanlog            = 2.5,
#   sdlog              = 1.2
# )
#
#
# run_pipeline(
#   disease_name       = "Marburg Virus Disease",
#   data               = NULL,
#   account_for_delay  = FALSE,
#   epidist            = NULL,
#   total_cases        = 70,
#   total_deaths       = 25,
#   death_in_confirmed = 12
# )


# disease_name       = "Marburg Virus Disease";
# data               = read.csv(system.file("extdata",
#                                           "Marburg_EqGuinea_linelist.csv",
#                                           package = "episoap"));
# account_for_delay  = TRUE;
# epidist            = NULL;
# date_variable_name = "Onset_week";
# cases_status       = "Status";
# death_outcome      = "dead";
# diagnosis_status   = "Type";
# diagnosis_outcome  = "confirmed";
# distribution       = "lnorm";
# meanlog            = 2.5;
# sdlog              = 1.2;
# type = NULL; values = NULL; interval=NULL; shape=NULL; scale=NULL
#

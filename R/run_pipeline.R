
# epidemic_phase = NULL,
# infection_type = "close_contact",
# scope          = NULL,
# study_question = "severity",


# run_pipeline(
#   disease_name      = "Marburg Virus Disease",
#   data              = readRDS(system.file("extdata", "Marburg_EqGuinea_incidence.RDS", package = "episoap")),
#   account_for_delay = FALSE,
#   epidist           = NULL
# )
#
#  run_pipeline(
#   disease_name      = "Marburg Virus Disease",
#   data              = read.csv(system.file("extdata",
#                                            "Marburg_EqGuinea_linelist.csv",
#                                             package = "episoap")),
#   account_for_delay = TRUE,
#   epidist           = NULL,
#   date_variable_name = "Onset_week",
#   cases_status       = "Status",
#   death_outcome      = "dead",
#   diagnosis_status   = "Type",
#   diagnosis_outcome  = "confirmed",
#   distribution       = "gamma",
#   type               = "range",
#   values             = c(8, 2, 16)
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


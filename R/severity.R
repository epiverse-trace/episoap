
## data preparation function - this will output the data in the form that
## cfr_static requires (done)
##
## have a function that uses this data and calls cfr_static (in progress)
##
## have another function to take variables from the user (in progress - will be
## combine with the above function)
##

#' Get the input data for {cfr} functions
#'
#' @param data
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
#' @export
#'
#' @examples
#' data = read.csv(system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                    package = "episoap"))
#' cfr_data <- prepare_cfr_data(
#'   data               = data,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed"
#' )
prepare_cfr_data <- function(data,
                             date_variable_name = "Onset_week",
                             cases_status       = "Status",
                             death_outcome      = "dead",
                             diagnosis_status   = "Type",
                             diagnosis_outcome  = "confirmed") {
  data[[date_variable_name]] <- as.factor(data[[date_variable_name]])
  cfr_data_all_cases         <- data %>%
    dplyr::group_by_at(date_variable_name) %>%
    dplyr::summarise(cases = dplyr::n(),
                     deaths = sum(.data[[cases_status]] == death_outcome,
                                 na.rm = TRUE))
    names(cfr_data_all_cases)[[1L]] <- "date"

  if (!is.null(diagnosis_status) && !is.null(diagnosis_outcome)) {
    cfr_data_confirmed_cases <- data %>%
      dplyr::group_by_at(date_variable_name) %>%
      dplyr::summarise(cases  = dplyr::n(),
                       deaths = sum(.data[[diagnosis_status]] == diagnosis_outcome & # nolint: line_length_linter
                                      .data[[cases_status]] == death_outcome,
                                    na.rm = TRUE))
    names(cfr_data_confirmed_cases)[[1L]] <- "date"
  } else {
    cfr_data_confirmed_cases <- NULL
  }

  list(
    cfr_data_all_cases       = cfr_data_all_cases,
    cfr_data_confirmed_cases = cfr_data_confirmed_cases
  )
}

#' Title
#'
#' @param data
#' @param account_for_delay
#' @param epidist
#' @param total_cases
#' @param total_death
#' @param death_in_confirmed
#'
#' @return
#' @export
#'
#' @examples
calculate_cfr <- function(data,
                          account_for_delay  = FALSE,
                          epidist            = NULL,
                          total_cases        = NULL,
                          total_death        = NULL,
                          death_in_confirmed = NULL) {

  # if account_for_delay==TRUE and epidist = NULL, get the delay distribution as
  # we did previously
  #
  # if the data does not contain the cases, and deaths, use the user-provided
  # total_cases, total_death, death_in_confirmed

}

#' Calculate CFR with uncertainty
#'
#' @param data a `data frame` or `linelist` object
#' @param infection_type the type of the infection
#' @param cases_status the name of the column with the information about whether
#'    a case was dead or recovered
#' @param outcomes a `character` vector with the 2 outcomes about the cases
#'    status. default is c("dead", "recovered")
#' @param diagnosis_status the name of the column with the cases diagnosis
#'    outcome.
#' @param diagnosis_outcome the value, in the column with the cases diagnosis
#'    outcome, used to represent the confirmed cases.
#'
#' @return a `list` with elements of type `vector`. Each vector contains the
#'    estimated CFR value and its confident interval.
#' \enumerate{
#'   \item cfr_total: the CFR among the total cases
#'   \item cfr_confirmed_only: the CFR among the confirmed cases only
#'   }
#'
#' @export
#'
#' @examples
#' overall_cfr <- calculate_overall_cfr(
#'   data              = system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                                   package = "readepi"),
#'   infection_type    = "direct_contact",
#'   cases_status      = "Status",
#'   outcomes          = c("dead", "recovered"),
#'   diagnosis_status  = "Type",
#'   diagnosis_outcome = "confirmed"
#' )
calculate_overall_cfr <- function(data,
                                  infection_type    = "direct_contact",
                                  cases_status      = "Status",
                                  outcomes          = c("dead", "recovered"),
                                  diagnosis_status  = "Type",
                                  diagnosis_outcome = "confirmed",
                                  ...) {

  ## the values of the ... argument could be:
  ## 1. the total number of death
  ## 2. the total number of confirmed cases
  ## 3. the number of death in the confirmed cases
  ##
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_choice(infection_type, choices = c("direct_contact",
                                                       "vector_borne",
                                                       "food_water_born"),
                           null.ok = FALSE)
  checkmate::assert_character(cases_status, null.ok = FALSE, len = 1L)
  checkmate::assert_vector(outcomes, null.ok  = FALSE, min.len = 1L,
                           unique = TRUE)
  checkmate::assert_character(diagnosis_status, null.ok = FALSE, len = 1L)
  checkmate::assert_character(diagnosis_outcome, null.ok = FALSE, len = 1L)

  ## check whether the input is incidence or linelist (data.frame)
  total_cases      <- nrow(data)
  deaths           <- nrow(data[data[[cases_status]] == outcomes[[1L]], ])
  confirmed        <- nrow(data[data[[diagnosis_status]] == diagnosis_outcome, ])
  confirmed_deaths <- nrow(data[which(data[[diagnosis_status]] == diagnosis_outcome & # nolint: line_length_linter
                                        data[[cases_status]] == outcomes[[1L]]), ]) # nolint: line_length_linter

  # CFR among total cases
  cfr_total          <- ci_text(deaths, total_cases)

  # CFR among confirmed cases
  cfr_confirmed_only <- ci_text(confirmed_deaths, confirmed)

  list(
    cfr_total          = cfr_total,
    cfr_confirmed_only = cfr_confirmed_only
  )
}

#' Get binomial confidence interval
#'
#' @param x the total number of deaths or confirmed deaths(for the CFR among the
#'    confirmed cases only)
#' @param n the total number of cases or confirmed cases(for the CFR among the
#'    confirmed cases only)
#'
#' @returns a `vector` of 3 elements of type numeric that correspond to the
#'    estimated CFR and its confidence interval.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' ci <- ci_text(120, 20)
ci_text <- function(x, n) {
  bin_out <- stats::binom.test(as.numeric(x), as.numeric(n))
  est     <- round(100.0 * c(bin_out[["estimate"]], bin_out[["conf.int"]]))
  c(est[[1L]], est[[2L]], est[[3L]])
}

#' Create the `epidist` object
#'
#' @param data the input data frame or linelist object
#' @param disease the name of the disease of interest
#' @param ... the other extra arguments. Possible values are:
#' \enumerate{
#'   \item type: a `character` specifying whether summary statistics are based
#'      around percentiles (default) or range. Type `?epiparemeter::extract_param` # nolint: line_length_linter
#'      for more details.
#'   \item values: a numeric `vector`. Type `?epiparemeter::extract_param` for
#'      more details
#'   \item distribution: a `character` string that specifies the distribution to
#'      use. Type `?epiparemeter::extract_param` for more details.
#'   \item shape: the shape of the specified distribution (when distribution = gamma or weibull) # nolint: line_length_linter
#'   \item scale: the scale of the specified distribution (when distribution = gamma or weibull) # nolint: line_length_linter
#'   \item meanlog: the meanlog of the specified distribution (when distribution = lnorm) # nolint: line_length_linter
#'   \item sdlog: the sdlog of the specified distribution (when distribution = lnorm) # nolint: line_length_linter
#'   }
#'
#' @return an object of type `epidist` with the epidemiological parameters of
#'    the disease of interest.
#' @export
#'
#' @examples
#' onset_death <- get_onset_to_death_distro(
#'   data         = data,
#'   disease      = "Marburg Virus Disease",
#'   type         = "range",
#'   values       = c(8, 2, 16),
#'   distribution = "gamma"
#' )
get_onset_to_death_distro <- function(data,
                                      disease      = "Marburg Virus Disease",
                                      ...) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_character(disease, null.ok = FALSE, len = 1L)

  args_list <- list(...) #distribution should be mandatory in this list

  # get the epidist object
  onset_death <- epiparameter::epidist_db(disease = disease,
                                          single_epidist = TRUE)
  if (!epiparameter::is_parameterised(onset_death)) {
    if (all(c("type", "values", "distribution") %in% names(args_list))) {
      param <- extract_params(data, args_list)
    } else if (any(c("shape", "scale", "meanlog", "sdlog") %in% names(args_list))) { # nolint: line_length_linter
      param <- get_params(args_list)
    } else {
      stop("Please provide the distribution parameters or summary statistics.")
    }

    if (args_list[["distribution"]] %in% c("gamma", "weibull")) {
      onset_death <- epiparameter::epidist(
        disease                  = disease,
        epi_dist                 = "onset_to_death",
        prob_distribution        = args_list[["distribution"]],
        prob_distribution_params = c(shape = param[["shape"]],
                                     scale = param[["scale"]])
      )
    } else {
      onset_death <- epiparameter::epidist(
        disease                  = disease,
        epi_dist                 = "onset_to_death",
        prob_distribution        = args_list[["distribution"]],
        prob_distribution_params = c(meanlog = param[["meanlog"]],
                                     sdlog   = param[["sdlog"]])
      )
    }
  }

  onset_death
}

#' Calculate CFR with uncertainty by accounting for the epidemiological delay
#' distribution of symptom onset to outcome.
#'
#' @param data the input data frame or linelist object
#' @param onset_death an object of type `epidist` with the epidemiological
#'    parameters of the disease of interest.
#' @param onset_date_variable the name of the column with the onset date values
#' @param deaths_date_variable the name of the column with the death date values
#' @param date_scale the time scale of the onset date values
#' @param cases_status the name of the column with the information about whether
#'    a case was dead or recovered
#' @param outcome the value in the `cases_status` column that is used to
#'    specify if the cases were dead. default is "dead".
#'
#' @return a vector of 3 elements: the estimated CFR and the confidence interval
#'    around it.
#' @export
#'
#' @examples
#' delay_cfr <- calculate_delay_cfr(
#'   data                   = data,
#'   onset_death            = onset_death,
#'   onset_date_variable    = "Onset_week",
#'   deaths_date_variable   = "date_death",
#'   date_scale             = "day",
#'   cases_status           = "Status",
#'   outcome                = "dead"
#' )
calculate_delay_cfr <- function(data, onset_death,
                                onset_date_variable    = "date_onset",
                                deaths_date_variable   = "date_death",
                                date_scale             = "day",
                                cases_status           = "Status",
                                outcome                = "dead") {
  checkmate::assert_character(onset_date_variable, null.ok = TRUE, len = 1L)
  checkmate::assert_character(date_scale, null.ok = TRUE, len = 1L)

  # depending on data_type, if linelist, create incidence object
  # if incidence, no transformation
  # but need to make sure the incidence object to be a data frame of 2 columns
  # named as 'date' and 'cases'.
  #
  if (inherits(data, "linelist")) {
    # we need to convert the linelist into incidence.
    # for now we will use days as time scale until {cfr} can account for other
    # time intervals like it is in {incidence2}
    #
    # {cfr} needs daily data. for this reason, we are converting our incidence
    # data into a daily incidence using incidence2::complete_dates. this needs
    # to be looked into

    ## ----- the following is only for test purpose. it should be removed
    data[[deaths_date_variable]] <- data[[onset_date_variable]] + 100L
    ## ----- end of it
    ## date_index = c(onset_date_variable,
    ## deaths_date_variable)
    incidence_data <- incidence2::incidence(data,
                                            date_index = c(onset_date_variable,
                                                           deaths_date_variable), # nolint: line_length_linter
                                            interval = date_scale) |>
      incidence2::complete_dates()
  }

  # use the cfr::prepare_data() to get the input for cfr::known_outcomes()
  data_for_cfr <- cfr::prepare_data(incidence_data,
                                    cases_variable  = onset_date_variable,
                                    deaths_variable = deaths_date_variable)

  # calculate CFR for known outcome cases
  known_outcomes_cfr  <- cfr::known_outcomes(data_for_cfr, onset_death)

  # Estimate cases with known outcomes
  total_known_to_date <- sum(known_outcomes_cfr[["known_outcomes"]])

  # estimate CFR with delay
  num_death <- nrow(data[which(data[[cases_status]] == outcome), ])
  delay_cfr <- ci_text(num_death, round(total_known_to_date))
  delay_cfr
}

#' Extract the distribution parameters from percentiles, median and range
#'
#' @param data the input data frame or linelist
#' @param arg_list a list with the parameters
#'
#' @keywords internal
#' @noRd
#'
extract_params <- function(data, arg_list) {
  checkmate::assert_list(arg_list, null.ok = FALSE, min.len = 1L)
  param <- epiparameter::extract_param(type         = arg_list[["type"]],
                                       values       = arg_list[["values"]],
                                       distribution = arg_list[["distribution"]], # nolint: line_length_linter
                                       samples      = nrow(data))
  param
}

#' Get the distribution parameters of interest
#'
#' @param arg_list a list with the parameters
#'
#' @keywords internal
#' @noRd
#'
get_params <- function(arg_list) {
  if (c("shape", "scale") %in% arg_list) {
    param        <- c(arg_list[["shape"]], arg_list[["scale"]])
    names(param) <- c("shape", "scale")
  } else if (c("meanlog", "sdlog") %in% arg_list) {
    param        <- c(arg_list[["meanlog"]], arg_list[["sdlog"]])
    names(param) <- c("meanlog", "sdlog")
  } else {
    stop("Incorrect parameters provided")
  }
  param
}

#' Print the CFR result
#'
#' @param cfr the CFR values obtained using either `calculate_delay_cfr` or
#'    `calculate_overall_cfr`
#'
#' @return displays the CFR result as a table
#' @export
#'
#' @examples
#' cfr <- calculate_overall_cfr(
#'   data              = system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                                   package = "readepi"),
#'   infection_type    = "direct_contact",
#'   cases_status      = "Status",
#'   outcomes          = c("dead", "recovered"),
#'   diagnosis_status  = "Type",
#'   diagnosis_outcome = "confirmed"
#' )
#' print(cfr)
#'
print_cfr <- function(cfr) {
  checkmate::assert_vector(cfr, len = 3L, any.missing = FALSE, null.ok = FALSE)
  cfr <- as.table(cfr)
  names(cfr) <- c("estimated_cfr", "lower_ci", "upper_ci")
  as.data.frame(unclass(t(cfr))) %>%
    dplyr::mutate(
      `estimated_cfr` = formattable::color_tile("white", "#81A4CE")(`estimated_cfr`)) %>% # nolint: line_length_linter
    knitr::kable("html", escape = FALSE, align = rep("c", length(cfr))) %>%
    kableExtra::kable_styling("hover", full_width = FALSE) %>%
    kableExtra::footnote(general = "Estimated CFR (c.f. {cfr} package)",
                         footnote_as_chunk = TRUE)
}

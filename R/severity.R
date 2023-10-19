
#' Calculate CFR with uncertainty
#'
#' @param infection_type the type of the infection
#' @param onset_date the name of the column with the onset date values
#' @param date_scale the time scale of the onset date values
#' @param data a data frame or a linelist object
#' @param death_status the name of the column with the case's death status
#' @param case_status
#'
#' @return
#' @export
#'
#' @examples
calculate_overall_cfr <- function(data,
                                  infection_type = "direct_contact",
                                  death_status   = "Status",
                                  case_status    = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_choice(infection_type, choices = c("direct_contact",
                                                       "vector_borne",
                                                       "food_water_born"),
                           null.ok = FALSE)
  checkmate::assert_character(death_status, null.ok = FALSE, len = 1L)
  checkmate::assert_character(case_status, null.ok = TRUE, len = 1L)

  ## check whether the input is incidence or linelist (data.frame)
  total_cases      <- nrow(data)
  deaths           <- nrow(data[data[[death_status]] == "dead", ])
  confirmed <- confirmed_deaths <- NULL
  if (!is.null(case_status)) {
    confirmed        <- nrow(data[data[[case_status]] == "confirmed", ])
    confirmed_deaths <- nrow(data[which(data[[case_status]] == "confirmed" &
                                          data[[death_status]] == "dead"), ])
  }


  # CFR among total cases
  cfr_total          <- ci_text(deaths, total_cases)

  # CFR among confirmed cases
  cfr_confirmed_only <- ci_text(confirmed_deaths, confirmed)

  list(
    cfr_total          = cfr_total,
    cfr_confirmed_only = cfr_confirmed_only
  )
}

# Define binomial confidence interval function:
ci_text <- function(x, n) {
  bin_out <- binom.test(as.numeric(x), as.numeric(n))
  est     <- round(100.0 * c(bin_out[["estimate"]], bin_out[["conf.int"]]))
  c(est[[1L]], est[[2L]], est[[3L]])
}

#' Title
#'
#' @param data
#' @param death_status
#' @param disease
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' onset_death <- get_onset_to_death_distro(
#'   data         = data,
#'   death_status = "Status",
#'   disease      = "Marburg Virus Disease",
#'   type         = "range",
#'   values       = c(8, 2, 16),
#'   distribution = "gamma"
#' )
get_onset_to_death_distro <- function(data,
                                      death_status = "Status",
                                      disease      = "Marburg Virus Disease",
                                      ...) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_character(death_status, null.ok = FALSE, len = 1L)
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
    } else{
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

#' Title
#'
#' @param data
#' @param onset_death
#' @param onset_date_variable
#' @param deaths_date_variable
#' @param date_scale
#' @param death_status
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
#' delay_cfr <- calculate_delay_cfr(
#'   data                   = data,
#'   onset_death            = onset_death,
#'   onset_date_variable    = "Onset_week",
#'   deaths_date_variable   = "date_death",
#'   date_scale             = "day",
#'   death_status           = "Status",
#'   outcome                = "dead"
#' )
calculate_delay_cfr <- function(data, onset_death,
                                onset_date_variable    = "date_onset",
                                deaths_date_variable   = "date_death",
                                date_scale             = "day",
                                death_status           = "Status",
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
    data[[deaths_date_variable]] = data[[onset_date_variable]] + 100
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
  num_death <- nrow(data[which(data[[death_status]] == outcome), ])
  delay_cfr <- ci_text(num_death, round(total_known_to_date))
  delay_cfr
}

extract_params <- function(data, arg_list) {
  type         = arg_list[["type"]]
  values       = arg_list[["values"]]
  distribution = arg_list[["distribution"]]
  param <- epiparameter::extract_param(type         = type,
                                       values       = values,
                                       distribution = distribution,
                                       samples      = nrow(data))
  param
}

get_params <- function(arg_list) {
  if (c("shape", "scale") %in% arg_list) {
    param <- c(arg_list[["shape"]], arg_list[["scale"]])
    names(param) <- c("shape", "scale")
  } else if (c("meanlog", "sdlog") %in% arg_list) {
    param <- c(arg_list[["meanlog"]], arg_list[["sdlog"]])
    names(param) <- c("meanlog", "sdlog")
  } else {
    stop("Incorrect parameters provided")
  }
  param
}



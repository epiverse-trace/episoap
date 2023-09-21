
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
  # paste(est[[1L]], "% (95% CI: ", est[[2L]], "-", est[[3L]], "%)", sep = "")
}

calculate_delay_cfr <- function(data,
                                onset_date   = "date_onset",
                                death_date   = "date_death",
                                date_scale   = "days",
                                death_status = "Status",
                                disease      = "Marburg Virus Disease",
                                ...) {
  checkmate::assert_character(onset_date, null.ok = TRUE, len = 1L)
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_character(death_status, null.ok = FALSE, len = 1L)
  checkmate::assert_character(date_scale, null.ok = TRUE, len = 1L)

  args_list <- list(...) #distribution should be mandatory in this list

  # get the epidist object
  onset_death <- epiparameter::epidist_db(disease = disease,
                                          single_epidist = TRUE)
  if (!epiparameter::is_parameterised(onset_death)) {
    if (all(c("type", "values", "distribution") %in% names(args_list))) {
      param <- extract_params(data, args_list)
    } else if (any(c("shape", "scale", "meanlog", "sdlog") %in% names(args_list))) {
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
                                     scale = param[["scale"]]))
    } else {
      onset_death <- epiparameter::epidist(
        disease                  = disease,
        epi_dist                 = "onset_to_death",
        prob_distribution        = args_list[["distribution"]],
        prob_distribution_params = c(meanlog = param[["meanlog"]],
                                     sdlog = param[["sdlog"]]))
    }
  }

  ## WIP
  # convert to incidence
  # incidence_data <- incidence2::incidence(data, c("Onset_week", "death_date")) |>
  #   incidence2::complete_dates()

  # calculate CFR for known outcome cases
  # data_for_cfr <- data %>%
  #   dplyr::select({{ onset_date }}, {{ death_date }})
  # names(data_for_cfr) <- c("cases", "date")
  # known_outcomes_cfr  <- cfr::known_outcomes(data_for_cfr, onset_death)
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



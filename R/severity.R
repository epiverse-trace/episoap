#' Calculate CFR from count data
#'
#' @param total_cases A \code{<numeric>} that represents the total number of
#'    known cases
#' @param total_deaths A \code{<numeric>} that represents the total number of
#'    known deaths
#' @param death_in_confirmed A \code{<numeric>} that represents the number of
#'    death among the confirmed cases.
#'
#' @return A \code{<tibble>} with one or two rows containing respectively the
#'    overall CFR and CFR in confirmed cases.
#' @export
#' @examples
#' cfr <- calculate_cfr_from_counts(
#'   total_cases = 1150,
#'   total_deaths = 12,
#'   death_in_confirmed = 2
#' )
#'
calculate_cfr_from_counts <- function(total_cases,
                                      total_deaths,
                                      death_in_confirmed) {

  # if the user does not have a data frame that contains the date, cases, and
  # deaths columns, but provided the number of total_cases, total_death,
  # death_in_confirmed, this function can be used to estimate CFR.

  # make sure that the parameters are converted into numeric
  total_cases <- as.numeric(total_cases)
  total_deaths <- as.numeric(total_deaths)

  # calculate the overall CFR
  cfr <- rstatix::binom_test(
    x = round(total_deaths),
    n = total_cases,
    p = 0.999
  )
  names(cfr)[2:4] <- c("severity_mean", "severity_low", "severity_high")
  cfr[["type"]] <- "CFR"

  # calculate the CFR among confirmed cases only
  if (!is.null(death_in_confirmed)) {
    death_in_confirmed <- as.numeric(death_in_confirmed)
    cfr_in_confirmed_cases <- rstatix::binom_test(
      x = round(death_in_confirmed),
      n = total_cases,
      p = 0.999
    )
    names(cfr_in_confirmed_cases)[2:4] <- c(
      "severity_mean", "severity_low", "severity_high"
    )
    cfr_in_confirmed_cases[["type"]] <- "CFR in confirmed cases"
    cfr <- rbind(cfr, cfr_in_confirmed_cases)
  }

  cfr <- cfr %>%
    dplyr::select(severity_mean, severity_low, severity_high, type)
  return(cfr)
}

#' Estimate disease severity from incidence object
#'
#' @inheritParams get_severity
#'
#' @return an object of type `list` with 2 data frames that contains the
#'    CFR values in all cases and in confirmed cases only.
#' @keywords internal
#'
calculate_cfr_from_incidence <- function(data,
                                         epidist,
                                         epidist_params) {

  # convert dates to sequential if necessary
  if (!(unique(diff(data[["date"]])) == 1L)) {
    data  <- get_sequential_dates(data)
  }

  # when the user did not provide epidist and epidist_params, then
  # the static CFR is returned without accounting for delay
  if (is.null(epidist) && is.null(epidist_params)) {
    cfr <- cfr::cfr_static(
      data = data,
      delay_density = NULL
    )
    return(cfr)
  }

  # when the user did has provided epidist, then
  # the static CFR is calculated by accounting for the provided delay
  # distribution
  if (!is.null(epidist)) {
    cfr <- cfr::cfr_static(
      data = data,
      delay_density = epidist
    )
    return(cfr)
  }

  # when the user did has provided epidist_params, then
  # the static CFR is calculated by accounting for the delay distribution that
  # is calculated from its parameters
  if (!is.null(epidist_params)) {
    params <- get_delay_distro_params(
      data,
      type = epidist_params[["type"]],
      values = epidist_params[["values"]],
      distribution = epidist_params[["distribution"]],
      shape = epidist_params[["shape"]],
      scale = epidist_params[["scale"]],
      meanlog = epidist_params[["meanlog"]],
      sdlog = epidist_params[["sdlog"]]
    )
    if (epidist_params[["distribution"]] %in% c("gamma", "weibull")) {
      onset_to_death_distribution <- distcrete::distcrete(
        name = "gamma",
        shape = params[["shape"]],
        scale = params[["scale"]],
        interval = epidist_params[["interval"]]
      )
      cfr <- cfr::cfr_static(
        data = data,
        delay_density = onset_to_death_distribution$d
      )
    } else {
      onset_to_death_distribution <- distributional::dist_lognormal(
        mu = params[["meanlog"]],
        sigma = params[["sdlog"]]
      )
      cfr <- cfr::cfr_static(
        data = data,
        delay_density = function(x) unlist(
          density(onset_to_death_distribution, x)
        )
      )
    }
  }

  return(cfr)
}

#' Calculate CFR from incidence or linelist data
#'
#' @param data The input data in the form of a linelist or incidence.
#' @param epidist an `epidist` object that contains the distribution parameters
#' @param epidist_params A list with the parameters to be used to get the delay
#'    distribution. Possible values are:
#'    \describe{
#'        type, values, distribution, interval, shape, scale, meanlog, sdlog
#'    }
#' @return A \code{<data.frame>} with two rows containing respectively the CFR
#'    across all and confirmed cases.
#' @export
#'
#' @examples
#' data <- read.csv(system.file("extdata", "Marburg_EqGuinea_linelist.csv",
#'                              package = "episoap"))
#' incidence_data <- convert_to_incidence(
#'   data = data,
#'   date_var_name = "Onset_week",
#'   cases_status_var_name = "Status",
#'   death_outcome = "dead",
#'   diagnosis_status_var_name = "Type",
#'   diagnosis_outcome = "confirmed"
#' )
#' cfr <- calculate_cfr(
#'   data = incidence_data,
#'   epidist = NULL
#' )
calculate_cfr <- function(data,
                          epidist = NULL,
                          epidist_params = NULL) {

  # when the input data is obtained from the transformation of a linelist into
  # incidence via the 'convert_to_incidence()' function, there is a chance that
  # it contains an attribute 'confirmed_cases_data'.
  # If this is the case, then we will calculate CFR for the overall cases then
  # for confirmed cases only using the same function.

  # calculate the overall CFR
  cfr <- calculate_cfr_from_incidence(data, epidist, epidist_params)
  cfr[["type"]] <- "overall CFR"

  # calculate CFR in confirmed cases only if needed
  confirmed_cases_data <- attr(data, "confirmed_cases_data")
  if (!is.null(confirmed_cases_data)) {
    cfr_in_confirmed_cases <- calculate_cfr_from_incidence(
      data = confirmed_cases_data,
      epidist,
      epidist_params
    )
    cfr_in_confirmed_cases[["type"]] <- "CFR in confirmed cases"
    cfr <- rbind(cfr, cfr_in_confirmed_cases)
  }

  return(cfr)
}


#' Convert non-daily incidence data into daily incidence data
#'
#' @param data A \code{<data.frame>} with at least two columns named as `cases`
#'    and the `date`
#'
#' @return A \code{<data.frame>} with three columns named as 'date', 'cases',
#'    'deaths'. This is the format required by the \code{cfr::cfr_static()}
#'    function.
#' @export
#' @examples
#' weekly_incidence <- readRDS(
#'   system.file("extdata", "weekly_incidence.RDS", package = "episoap")
#' )
#' daily_incidence <- get_sequential_dates(weekly_incidence)
#'
get_sequential_dates <- function(data) {
  # convert the date column into Date
  data[["date"]] <- as.Date(as.character(data[["date"]]))

  # get the column number with the date information
  idx <- match("date", names(data))

  # generate the daily sequence of date between the first and last date and
  # convert it into a data frame
  date <- seq.Date(
    as.Date(data[["date"]][[1L]]),
    as.Date(data[["date"]][nrow(data)]),
    1
  )
  seq_date <- as.data.frame(date)

  # create the remaining columns of the input data within the new daily
  # incidence data frame
  for (nms in names(data)[-idx]) {
    seq_date[[nms]] <- NA
  }

  # get the indices of the dates present in the input data
  row_idx <- match(data[["date"]], seq_date[["date"]])

  # get the names of the columns other than the date column from the input data,
  # and their indices
  other_column_names <- names(data)[-idx]
  idx_other_columns <- match(other_column_names, names(data))

  # fill in the other columns in the new daily incidence data with their
  # corresponding values from the input data.
  tmp <- data %>% dplyr::select(-c(date))
  seq_date[row_idx, idx_other_columns] <- tmp
  seq_date <- seq_date %>%
    dplyr::select(c(date, cases, deaths))
  seq_date[is.na(seq_date[["cases"]]), ][["cases"]] <- 0
  seq_date[is.na(seq_date[["deaths"]]), ][["deaths"]] <- 0

  return(seq_date)
}



#' Get the delay distribution parameters
#'
#' @inheritParams get_severity
#'
#' @return an object an type `list` with the delay distribution parameters
#' @keywords internal
#'
get_delay_distro_params <- function(tmp_data,
                                    type, values, distribution,
                                    shape, scale,
                                    meanlog, sdlog) {
  if (all(!is.null(type) && !is.null(values) && !is.null(distribution))) {
    args_list <- list(type         = type,
                      values       = values,
                      distribution = distribution)
    param     <- extract_params(tmp_data, args_list)
  } else if (!is.null(shape) && !is.null(scale)) {
    param <- list(shape = shape, scale = scale)
  } else if (!is.null(meanlog) && !is.null(sdlog)) {
    param <- list(meanlog = meanlog, sdlog = sdlog)
  } else {
    stop("Please provide the distribution parameters or summary statistics.")
  }
  return(param)
}



#' Create the delay distribution object
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
#'   \item interval: the interval to discretise the interval onto. Default is 1
#'      for daily case and death data.
#'   }
#'
#' @return an object of type `epidist` with the epidemiological parameters of
#'    the disease of interest.
#' @keywords internal
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
                                      disease = "Marburg Virus Disease",
                                      ...) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_character(disease, null.ok = FALSE, len = 1L)

  args_list <- list(...) #distribution should be mandatory in this list

  # get the epidist object
  if (all(c("type", "values", "distribution") %in% names(args_list))) {
    param <- extract_params(data, args_list)
  } else if (any(c("shape", "scale", "meanlog", "sdlog") %in% names(args_list))) { # nolint: line_length_linter
    param <- get_params(args_list)
  } else {
    stop("Please provide the distribution parameters or summary statistics.")
  }

  if (args_list[["distribution"]] %in% c("gamma", "weibull")) {
    onset_death <- distcrete::distcrete(
      name     = args_list[["distribution"]],
      shape    = param[["shape"]],
      scale    = param[["scale"]],
      interval = args_list[["interval"]]
    )
  } else {
    onset_death <- distributional::dist_lognormal(mu    = param[["meanlog"]],
                                                  sigma = param[["sdlog"]])
  }
  onset_death
}


#' Extract the distribution parameters from percentiles, median and range
#'
#' @param data the input data frame or linelist
#' @param arg_list a list with the parameters
#'
#' @keywords internal
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
#' @param cfr the CFR values obtained from the `get_severity()` function
#'
#' @return displays the CFR result as a table
#' @export
#'
#' @examples
#' cfr <- get_severity(
#'   disease_name = "Marburg Virus Disease",
#'   data         = readRDS(system.file("extdata",
#'                                       "Marburg_EqGuinea_incidence.RDS",
#'                                       package = "episoap")),
#'   account_for_delay = FALSE,
#'   epidist           = NULL
#' )
#' print(cfr)
#'
reformat_cfr <- function(cfr) {
  if (inherits(cfr, "vector")) {
    cfr <- as.table(cfr)
    names(cfr) <- c("estimated_cfr", "lower_ci", "upper_ci")
    out <- as.data.frame(unclass(t(cfr))) %>%
      dplyr::mutate(`estimated_cfr` =
                      formattable::color_tile("white",
                                              "#81A4CE")(`estimated_cfr`))
  }

  if (inherits(cfr, "data.frame") && ncol(cfr) == 3L) {
    out <- cfr %>%
      dplyr::mutate(`severity_mean` =
                      formattable::color_tile("white",
                                              "#81A4CE")(`severity_mean`))
  }

  if (inherits(cfr, "list")) {
    out <- list()
    for (res in names(cfr)) {
      out[[res]] <- cfr[[res]] %>%
        dplyr::mutate(`severity_mean` =
                        formattable::color_tile("white",
                                                "#81A4CE")(`severity_mean`))

    }
  }
  out
}

print_cfr <- function(out) {
  # the problem is related to the fact that kableExtra function's output do not
  # show in the html file after rendering.
  # Possibly try to have a .css file
  if (inherits(out, "list")) {
    for (res in names(out)) {
      print(knitr::kable(out[[res]], format = "html", escape = FALSE, booktabs = TRUE,
                   align = rep("c", ncol(out[[res]])),
                   caption = res,
                   digits = 5))

      cat("\n")
    }
  }
}

#' Estimate severity based on the user-provided arguments
#'
#' @param disease_name a `character` with the name of the disease
#' @param data the input data
#' @param account_for_delay a `logical` used to specify whether to account for
#'    the delay distribution or not.
#' @param epidist an object of class `epidist` with the delay distribution
#'    parameters
#' @param ... the extra parameters needed for some specific cases
#'
#' @export
#' @returns a list of data frames that contains the severity estimates.
#'
#' @examples
#' data <- readRDS(
#'   system.file(
#'     "extdata", "Marburg_EqGuinea_incidence.RDS", package = "episoap"
#'   )
#' )
#' cfr <- get_severity(
#'   disease_name = "Marburg Virus Disease",
#'   data = data,
#'   account_for_delay = FALSE,
#'   epidist = NULL
#' )
#'
get_severity <- function(disease_name = NULL,
                         data = NULL,
                         account_for_delay = FALSE,
                         epidist = NULL,
                         ...) {

  # get the additional arguments
  args_list <- list(...)

  # get the delay distribution parameters if the delay distribution is
  # not provided
  type <- args_list[["type"]]
  values <- args_list[["values"]]
  distribution <- args_list[["distribution"]]
  interval <- args_list[["interval"]]
  meanlog <- args_list[["meanlog"]]
  sdlog <- args_list[["sdlog"]]
  shape <- args_list[["shape"]]
  scale <- args_list[["scale"]]

  # estimate CFR from count data , "death_in_confirmed"
  total_cases <- args_list[["total_cases"]]
  total_deaths <- args_list[["total_deaths"]]
  death_in_confirmed <- args_list[["death_in_confirmed"]]
  if (all(!is.null(total_cases) && !is.null(total_deaths))) {
    message("Estimating severity from count data...")
    cfr_res <- calculate_cfr_from_counts(
      total_cases = total_cases,
      total_deaths = total_deaths,
      death_in_confirmed = death_in_confirmed
    )
    return(cfr_res)
  }

  if (all(c("date", "cases", "deaths") %in% names(data))) {
    data <- data %>%
      dplyr::select(c(date, cases, deaths))
    cfr_res <- calculate_cfr(
      data = data,
      epidist = epidist,
      account_for_delay = account_for_delay,
      type, values, distribution, interval,
      shape, scale,
      meanlog, sdlog
    )
    return(cfr_res)
  }
}


#' Extract the severity parameters from the input arguments
#'
#' @param parameters a list of parameters to be populated with the severity
#'    parameters
#' @param severity_params a list with the user-specified arguments to be used
#'    for the CFR calculation
#'
#' @return the input list of parameters with extra arguments from the
#'    'severity_params' object
#' @keywords internal
#'
get_severity_params <- function(parameters, severity_params) {
  if (!is.null(severity_params)) {
    parameters[["ACCOUNT_FOR_DELAY"]] <- severity_params[["account_for_delay"]]
    parameters[["EPIDIST"]]           <- severity_params[["epidist"]]

    # get parameters needed to estimate CFR from count
    if (all(c("total_cases", "total_deaths") %in% names(severity_params))) {
      death_in_confirmed   <- NULL
      if ("death_in_confirmed" %in% names(severity_params)) {
        death_in_confirmed <- severity_params[["death_in_confirmed"]]
      }
      parameters[["TOTAL_CASES"]]  <- severity_params[["total_cases"]]
      parameters[["TOTAL_DEATHS"]] <- severity_params[["total_deaths"]]
    }

    # get parameters needed to calculate the delay distribution
    if (all(c("type", "values", "distribution") %in% names(severity_params))) {
      parameters[["TYPE"]]         <- severity_params[["type"]]
      parameters[["VALUES"]]       <- severity_params[["values"]]
      parameters[["DISTRIBUTION"]] <- severity_params[["distribution"]]
      parameters[["INTERVAL"]]     <- severity_params[["interval"]]
    }
    if (all(c("meanlog", "sdlog", "distribution") %in% names(severity_params))) {
      parameters[["MEANLOG"]]      <- severity_params[["meanlog"]]
      parameters[["SDLOG"]]        <- severity_params[["sdlog"]]
      parameters[["DISTRIBUTION"]] <- severity_params[["distribution"]]
    }
  }
  return(parameters)
}

#' Run severity pipeline
#'
#' @examples
#' estimate cfr using incidence data
#' run_severity(
#'   disease_name      = "Marburg Virus Disease",
#'   data              = readRDS(system.file("extdata",
#'                                           "Marburg_EqGuinea_incidence.RDS",
#'                                           package = "episoap")),
#'   account_for_delay = FALSE,
#'   epidist           = NULL
#' )
#'
#' estimate cfr using linelist data - the delay distribution parameters are
#' taken from the user-supplied arguments
#' run_severity(
#'   disease_name       = "Marburg Virus Disease",
#'   data               = read.csv(system.file("extdata",
#'                                             "Marburg_EqGuinea_linelist.csv",
#'                                             package = "episoap")),
#'   account_for_delay  = TRUE,
#'   epidist            = NULL,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed",
#'   distribution       = "gamma",
#'   type               = "range",
#'   values             = c(8, 2, 16),
#'   interval           = 1
#' )
#'
#' run_severity(
#'   disease_name       = "Marburg Virus Disease",
#'   data               = read.csv(system.file("extdata",
#'                                             "Marburg_EqGuinea_linelist.csv",
#'                                             package = "episoap")),
#'   account_for_delay  = TRUE,
#'   epidist            = NULL,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed",
#'   distribution       = "lnorm",
#'   meanlog            = 2.5,
#'   sdlog              = 1.2
#' )
#'
#' estimate cfr using count data
#' run_severity(
#'   disease_name       = "Marburg Virus Disease",
#'   data               = NULL,
#'   account_for_delay  = FALSE,
#'   epidist            = NULL,
#'   total_cases        = 70,
#'   total_deaths       = 25,
#'   death_in_confirmed = 12
#' )
#' @export
#'
run_severity <- function(disease_name,
                         data,
                         account_for_delay = TRUE,
                         epidist = NULL,
                         ...) {
  args_list  <- list(...)

  cfr <- get_severity(
    disease_name       = disease_name,
    data               = data,
    account_for_delay  = account_for_delay,
    epidist            = epidist,
    values             = args_list[["values"]],
    type               = args_list[["type"]],
    interval           = args_list[["interval"]],
    distribution       = args_list[["distribution"]],
    shape              = args_list[["shape"]],
    scale              = args_list[["scale"]],
    meanlog            = args_list[["meanlog"]],
    sdlog              = args_list[["sdlog"]],
    total_cases        = args_list[["total_cases"]],
    total_deaths       = args_list[["total_deaths"]],
    death_in_confirmed = args_list[["death_in_confirmed"]]
  )
}

# cavits:
# printing function only show 1 result if it is a list
# need the defaults arguments when estimating cfr from count data

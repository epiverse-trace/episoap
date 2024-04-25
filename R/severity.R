#' Calculate CFR from count data
#'
#' @param total_cases a `numeric` that represent the total number of cases in
#'    the data
#' @param total_deaths a `numeric` that represents the total number of deaths in
#'    the data
#' @param death_in_confirmed a `numeric` that represents the number of death in
#'    among the confirmed cases.
#'
#' @return an object of type `list` with 2 elements of type `data frame`.
#' @keywords internal
#'
calculate_cfr_from_counts <- function(total_cases,
                                      total_deaths,
                                      death_in_confirmed) {

  # if the data does not contain the date, cases, and deaths columns, use the
  # user-provided total_cases, total_death, death_in_confirmed
  # could try a data frame

  res_cfr          <- list()
  total_cases      <- as.numeric(total_cases)
  total_deaths     <- as.numeric(total_deaths)
  severity_mean    <- total_deaths / total_cases
  severity_conf    <- stats::binom.test(round(total_deaths), total_cases,
                                        p = 1L)
  severity_lims    <- severity_conf[["conf.int"]]
  res_cfr[["cfr"]] <- data.frame(severity_mean = severity_mean,
                                 severity_low  = severity_lims[[1L]],
                                 severity_high = severity_lims[[2L]])
  if (!is.null(death_in_confirmed)) {
    severity_mean  <- death_in_confirmed / total_cases
    severity_conf  <- stats::binom.test(round(death_in_confirmed),
                                        total_cases, p = 1L)
    severity_lims  <- severity_conf[["conf.int"]]
    res_cfr[["cfr_in_confirmed_cases"]] <- data.frame(
      severity_mean = severity_mean,
      severity_low  = severity_lims[[1L]],
      severity_high = severity_lims[[2L]]
    )
  }

  res_cfr
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

#' Estimate disease severity from linelist object
#'
#' @inheritParams get_severity
#'
#' @return an object of type `list` with 1 or 2 data frames that contains the
#'    CFR values in all cases and in confirmed cases only.
#' @keywords internal
#'
calculate_cfr_from_linelist <- function(data, account_for_delay, epidist,
                                        type, values, distribution, interval,
                                        shape, scale,
                                        meanlog, sdlog) {
  # initialize the output object
  res_cfr      <- list()
  output_names <- c("cfr", "cfr_in_confirmed_cases")
  j            <- 1L
  for (i in c("all_cases_data", "confirmed_cases_data")) {
    tmp_data   <- data[[i]]

    # convert dates to sequential if necessary
    if (!identical(unique(diff(tmp_data[["date"]])), 1L)) {
      tmp_data <- get_sequential_dates(tmp_data)
    }

    # calculate CFR using the provided delay distribution
    if (account_for_delay && !is.null(epidist)) {
      res_cfr[[output_names[j]]] <- cfr::cfr_static(
        data          = tmp_data,
        delay_density = epidist
      )
      j <- j + 1L
    } else if (account_for_delay && is.null(epidist)) {
      # calculate CFR using the delay distribution parameters
      param <- get_delay_distro_params(tmp_data,
                                       type, values, distribution,
                                       shape, scale,
                                       meanlog, sdlog)

      if (distribution %in% c("gamma", "weibull")) {
        onset_to_death_distribution <- distcrete::distcrete(
          name     = "gamma",
          shape    = param[["shape"]],
          scale    = param[["scale"]],
          interval = interval
        )
        res_cfr[[output_names[j]]] <- cfr::cfr_static(
          data          = tmp_data,
          delay_density = onset_to_death_distribution$d
        )
      } else {
        onset_to_death_distribution <- distributional::dist_lognormal(
          mu    = param[["meanlog"]],
          sigma = param[["sdlog"]]
        )
        res_cfr[[output_names[j]]] <- cfr::cfr_static(
          data          = tmp_data,
          delay_density = function(x) unlist(density(onset_to_death_distribution, x)) # nolint: line_length_linter
        )
      }
      j <- j + 1L
    }
  }

  return(res_cfr)
}

#' Estimate disease severity from incidence object
#'
#' @inheritParams get_severity
#'
#' @return an object of type `list` with 2 data frames that contains the
#'    CFR values in all cases and in confirmed cases only.
#' @keywords internal
#'
calculate_cfr_from_incidence <- function(data, epidist, account_for_delay,
                                         type, values, distribution, interval,
                                         shape, scale,
                                         meanlog, sdlog) {
  res_cfr <- list()
  if (account_for_delay && !is.null(epidist)) {
    res_cfr[["cfr"]] <- cfr::cfr_static(
      data          = data,
      delay_density = epidist
    )
  } else if (account_for_delay && is.null(epidist)) {
    param <- get_delay_distro_params(data,
                                     type, values, distribution,
                                     shape, scale,
                                     meanlog, sdlog)
    if (distribution %in% c("gamma", "weibull")) {
      onset_to_death_distribution <- distcrete::distcrete(
        name     = "gamma",
        shape    = param[["shape"]],
        scale    = param[["scale"]],
        interval = interval
      )
      res_cfr[["cfr"]] <- cfr::cfr_static(
        data          = data,
        delay_density = onset_to_death_distribution$d
      )
    } else {
      onset_to_death_distribution <- distributional::dist_lognormal(
        mu    = param[["meanlog"]],
        sigma = param[["sdlog"]]
      )
      res_cfr[["cfr"]] <- cfr::cfr_static(
        data          = data,
        delay_density = function(x) unlist(density(onset_to_death_distribution,
                                                   x))
      )
    }
  } else {
    res_cfr[["cfr"]] <- cfr::cfr_static(
      data          = data,
      delay_density = NULL
    )
  }

  res_cfr[["cfr_in_confirmed_cases"]] <- NULL

  return(res_cfr)
}

#' Calculate CFR from incidence or linelist data
#'
#' @param data the input data
#' @param epidist an `epidist` object that contains the distribution parameters
#'
#' @return a `list` of the following 2 elements:
#' \enumerate{
#'   \item cfr: the CFR among the total cases
#'   \item cfr_in_confirmed_cases: the CFR among the confirmed cases only
#' }
#' @keywords internal
#'
#' @examples
#' cfr_data <- convert_to_incidence(
#'   data               = data,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed"
#' )
#' cfr <- calculate_cfr_from_incidence(
#'   data              = cfr_data,
#'   epidist           = NULL
#' )
calculate_cfr <- function(data,
                          epidist           = NULL,
                          account_for_delay = FALSE,
                          type, values, distribution, interval,
                          shape, scale,
                          meanlog, sdlog) {

  if (is.list(data) && all(names(data) %in% c("all_cases_data",
                                              "confirmed_cases_data"))) {
    res_cfr <- calculate_cfr_from_linelist(data, account_for_delay, epidist,
                                           type, values, distribution, interval,
                                           shape, scale,
                                           meanlog, sdlog)
  } else if (is.data.frame(data) && all(c("date", "cases", "deaths") %in%
                                        names(data))) {
    # convert dates to sequential if necessary
    if (!(unique(diff(data[["date"]])) == 1L)) {
      data  <- get_sequential_dates(data)
    }
    res_cfr <- calculate_cfr_from_incidence(
      data, epidist, account_for_delay,
      type, values, distribution, interval,
       shape, scale,
       meanlog, sdlog
    )
  } else {
    stop("Incorrect input data format...\n",
         "'data' should be a data frame with 3 columns named as 'date',",
         "'cases' and 'deaths'.\n Alternatively, use the 'prepare_cfr_data'",
         "function to create the input object.")
  }

  res_cfr
}


#' Convert date in input data for CFR calculation into sequential dates
#'
#' @param data a data frame with at least 2 columns: the `cases` and the `date`
#'    they were recorded.
#'
#' @return a data frame with 3 columns named as 'date', 'cases', 'deaths'. This
#'    is the format required by the `cfr::cfr_static()` function
#' @keywords internal
#'
get_sequential_dates <- function(data) {
  idx  <- match("date", names(data))
  date <- seq.Date(as.Date(data[["date"]][[1L]]),
                   as.Date(data[["date"]][nrow(data)]),
                   1)
  seq_date <- as.data.frame(date)
  for (nms in names(data)[-idx]) {
    seq_date[[nms]] <- NA
  }
  row_idx  <- match(data[["date"]], seq_date[["date"]])
  nms      <- names(data)[-idx]
  col_idx  <- match(nms, names(data))
  tmp <- data %>% dplyr::select(-c(date))
  seq_date[row_idx, col_idx] <- tmp
  seq_date <- seq_date %>%
    dplyr::select(c(date, cases, deaths))
  seq_date[which(is.na(seq_date[["cases"]])),][["cases"]] = 0L
  seq_date[which(is.na(seq_date[["deaths"]])),][["deaths"]] = 0L
  seq_date
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
#' cfr <- get_severity(
#'   disease_name = "Marburg Virus Disease",
#'   data         = readRDS(system.file("extdata",
#'                                       "Marburg_EqGuinea_incidence.RDS",
#'                                       package = "episoap")),
#'   account_for_delay = FALSE,
#'   epidist           = NULL
#' )
#'
get_severity <- function(disease_name      = NULL,
                         data              = NULL,
                         account_for_delay = FALSE,
                         epidist           = NULL,
                         ...) {

  # get the additional arguments
  args_list <- list(...)

  # get the delay distribution parameters if the delay distribution is
  # not provided
  type         <- args_list[["type"]]
  values       <- args_list[["values"]]
  distribution <- args_list[["distribution"]]
  interval     <- args_list[["interval"]]
  meanlog      <- args_list[["meanlog"]]
  sdlog        <- args_list[["sdlog"]]
  shape        <- args_list[["shape"]]
  scale        <- args_list[["scale"]]

  # estimate CFR from count data , "death_in_confirmed"
  total_cases        <- args_list[["total_cases"]]
  total_deaths       <- args_list[["total_deaths"]]
  death_in_confirmed <- args_list[["death_in_confirmed"]]
  if (all(!is.null(total_cases) && !is.null(total_deaths))) {
    message("Estimating severity from count data...")
    cfr_res          <- calculate_cfr_from_counts(
      total_cases        = total_cases,
      total_deaths       = total_deaths,
      death_in_confirmed = death_in_confirmed
    )
    return(cfr_res)
  }

  if (all(c("date", "cases", "deaths") %in% names(data))) {
    data <- data %>%
      dplyr::select(c(date, cases, deaths))
    cfr_res <- calculate_cfr(
      data              = data,
      epidist           = epidist,
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

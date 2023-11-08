
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
#' @keywords internal
#' @noRd
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
    cfr_data_all_cases[["date"]]    <- as.Date(
      as.character(cfr_data_all_cases[["date"]])
    )

  if (!is.null(diagnosis_status) && !is.null(diagnosis_outcome)) {
    cfr_data_confirmed_cases <- data %>%
      dplyr::group_by_at(date_variable_name) %>%
      dplyr::summarise(cases  = dplyr::n(),
                       deaths = sum(.data[[diagnosis_status]] == diagnosis_outcome & # nolint: line_length_linter
                                      .data[[cases_status]] == death_outcome,
                                    na.rm = TRUE))
    names(cfr_data_confirmed_cases)[[1L]] <- "date"
    cfr_data_confirmed_cases[["date"]]    <- as.Date(
      as.character(cfr_data_confirmed_cases[["date"]])
    )
  } else {
    cfr_data_confirmed_cases <- NULL
  }

  list(
    cfr_data_all_cases       = cfr_data_all_cases,
    cfr_data_confirmed_cases = cfr_data_confirmed_cases
  )
}


#' Calculate CFR from count data
#'
#' @param total_cases a `numeric` that represent the total number of cases in
#'    the data
#' @param total_deaths a `numeric` that represents the total number of deaths in
#'    the data
#' @param death_in_confirmed a `numeric` that represents the number of death in
#'    among the confirmed cases.
#'
#' @return
#' @keywords internal
#' @noRd
#'
#' @examples
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

#' Calculate CFR from incidence data
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
#' @noRd
#'
#' @examples
#' cfr_data <- prepare_cfr_data(
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
calculate_cfr_from_incidence <- function(data, epidist = NULL) {
  # initialise the output object
  res_cfr      <- list()
  output_names <- c("cfr", "cfr_in_confirmed_cases")

  if (is.list(data) && all(names(data) %in% c("cfr_data_all_cases",
                                           "cfr_data_confirmed_cases"))) {
    j <- 1L
    for (i in c("cfr_data_all_cases", "cfr_data_confirmed_cases")) {
      tmp_data <- data[[i]]

      # convert dates to sequential if necessary
      if (!identical(unique(diff(tmp_data[["date"]])), 1L)) {
        tmp_data <- get_sequential_dates(tmp_data)
      }

      # calculate CFR
      res_cfr[[output_names[j]]] <- cfr::cfr_static(
        data              = tmp_data,
        epidist           = epidist,
        poisson_threshold = 100L
      )
      j <- j + 1L
    }
  } else if (is.data.frame(data) && all(c("date", "cases", "deaths") %in%
                                        names(data))) {

    # convert dates to sequential if necessary
    if (!(unique(diff(data[["date"]])) == 1L)) {
      # this function is based on `incidence2` but it is not actually doing
      # what we want. Need to build a function for this.
      data <- get_sequential_dates(data)
    }
    res_cfr[["cfr"]] <- cfr::cfr_static(data              = data,
                                        epidist           = epidist,
                                        poisson_threshold = 100L)
    res_cfr[["cfr_in_confirmed_cases"]] <- NULL
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
#' @noRd
#'
#' @examples
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
#' @keywords internal
#' @noRd
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
#' @param cfr the CFR values obtained from the `get_severity()` function
#'
#' @return displays the CFR result as a table
#' @export
#'
#' @examples
#' cfr <- get_severity(
#'   data              = read.csv(system.file("extdata",
#'                                            "Marburg_EqGuinea_linelist.csv",
#'                                             package = "episoap")),
#'   infection_type    = "direct_contact",
#'   cases_status      = "Status",
#'   outcomes          = c("dead", "recovered"),
#'   diagnosis_status  = "Type",
#'   diagnosis_outcome = "confirmed"
#' )
#' print(cfr)
#'
print_cfr <- function(cfr) {

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
    for (res in names(cfr)) {
      out <- cfr[[res]] %>%
        dplyr::mutate(`severity_mean` =
                        formattable::color_tile("white",
                                                "#81A4CE")(`severity_mean`))
    }
  }
  out
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
#' # example code
#'
#'
get_severity <- function(disease_name      = NULL,
                         data              = NULL,
                         account_for_delay = FALSE,
                         epidist           = NULL,
                         ...) {

  # get the additional arguments
  args_list <- list(...)

  # get the delay distribution if not provided
  if (account_for_delay && is.null(epidist)) {
    type         <- args_list[["type"]]
    values       <- args_list[["values"]]
    distribution <- args_list[["distribution"]]
    if (all(!(is.null(type) && !is.null(values) && !is.null(distribution)))) {
      epidist <- get_onset_to_death_distro(
        data         = data,
        disease      = disease_name,
        type         = type,
        values       = values,
        distribution = distribution)
    } else {
      epidist <- get_onset_to_death_distro(data    = data,
                                           disease = disease_name)
    }
  }

  # estimate CFR from count data , "death_in_confirmed"
  total_cases        <- args_list[["total_cases"]]
  total_deaths       <- args_list[["total_deaths"]]
  death_in_confirmed <- args_list[["death_in_confirmed"]]
  if (all(!is.null(total_cases) && !is.null(total_deaths))) {
    message("Estimating severity from count data...")
    cfr_res <- calculate_cfr_from_counts(
      total_cases        = total_cases,
      total_deaths       = total_deaths,
      death_in_confirmed = death_in_confirmed
    )
    return(cfr_res)
  }

  # aggregate the data if it does not contain the 'date', 'cases', and 'deaths'
  # columns
  date_variable_name <- args_list[["date_variable_name"]]
  cases_status       <- args_list[["cases_status"]]
  death_outcome      <- args_list[["death_outcome"]]
  diagnosis_status   <- args_list[["diagnosis_status"]]
  diagnosis_outcome  <- args_list[["diagnosis_outcome"]]
  if (all(!is.null(date_variable_name) && !is.null(cases_status) &&
          !is.null(death_outcome) && !is.null(diagnosis_status) &&
          !is.null(diagnosis_outcome))) {
    cfr_data <- prepare_cfr_data(
      data               = data,
      date_variable_name = date_variable_name,
      cases_status       = cases_status,
      death_outcome      = death_outcome,
      diagnosis_status   = diagnosis_status,
      diagnosis_outcome  = diagnosis_outcome
    )
    cfr_res <- calculate_cfr_from_incidence(
      data               = cfr_data,
      epidist            = epidist
    )
    return(cfr_res)
  }

  if (all(c("date", "cases", "deaths") %in% names(data))) {
    data <- data %>%
      dplyr::select(c(date, cases, deaths))
    cfr_res <- calculate_cfr_from_incidence(
      data              = data,
      epidist           = epidist
    )
    return(cfr_res)
  }
}

#' @examples
#' estimate cfr using incidence data
#' run_pipeline(
#'   disease_name      = "Marburg Virus Disease",
#'   data              = readRDS(system.file("extdata",
#'                                           "Marburg_EqGuinea_incidence.RDS",
#'                                           package = "episoap")),
#'   account_for_delay = FALSE,
#'   epidist           = NULL
#' )
#'
#' estimate cfr using linelist data
#' run_pipeline(
#'   disease_name      = "Marburg Virus Disease",
#'   data              = read.csv(system.file("extdata",
#'                                            "Marburg_EqGuinea_linelist.csv",
#'                                             package = "episoap")),
#'   account_for_delay = TRUE,
#'   epidist           = NULL,
#'   date_variable_name = "Onset_week",
#'   cases_status       = "Status",
#'   death_outcome      = "dead",
#'   diagnosis_status   = "Type",
#'   diagnosis_outcome  = "confirmed",
#'   distribution       = "gamma",
#'   type               = "range",
#'   values             = c(8, 2, 16)
#' )
#'
#' estimate cfr using count data
#' run_pipeline(
#'   disease_name       = "Marburg Virus Disease",
#'   data               = NULL,
#'   account_for_delay  = FALSE,
#'   epidist            = NULL,
#'   total_cases        = 70,
#'   total_deaths       = 25,
#'   death_in_confirmed = 12
#' )
#'
run_pipeline <- function(disease_name,
                         data,
                         account_for_delay = TRUE,
                         epidist = NULL,
                         ...) {
  args_list <- list(...)
  parameters    <- list(
    DISEASE_NAME       = disease_name,
    DATA               = data,
    ACCOUNT_FOR_DELAY  = account_for_delay,
    EPIDIST            = epidist
  )

  # get parameters needed to estimate CFR from a data frame
  if (all(c("date_variable_name", "cases_status", "death_outcome",
            "diagnosis_status", "diagnosis_outcome") %in% names(args_list))) {
    parameters[["DATE_VARIABLE_NAME"]] <- args_list[["date_variable_name"]]
    parameters[["CASES_STATUS"]]       <- args_list[["cases_status"]]
    parameters[["DEATH_OUTCOME"]]      <- args_list[["death_outcome"]]
    parameters[["DIAGNOSIS_STATUS"]]   <- args_list[["diagnosis_status"]]
    parameters[["DIAGNOSIS_OUTCOME"]]  <- args_list[["diagnosis_outcome"]]
  }

  # get parameters needed to estimate CFR from count
  if (all(c("total_cases", "total_deaths") %in% names(args_list))) {
    death_in_confirmed <- NULL
    if ("death_in_confirmed" %in% names(args_list)) {
      death_in_confirmed <- args_list[["death_in_confirmed"]]
    }
    parameters[["TOTAL_CASES"]]  <- args_list[["total_cases"]]
    parameters[["TOTAL_DEATHS"]] <- args_list[["total_deaths"]]
  }

  # get parameters needed to calculate the delay distribution
  if (all(c("type", "values", "distribution") %in% names(args_list))) {
    parameters[["TYPE"]]         <- args_list[["type"]]
    parameters[["VALUES"]]       <- args_list[["values"]]
    parameters[["DISTRIBUTION"]] <- args_list[["distribution"]]
  }

  # estimate CFR
  # system.file("rmarkdown", "test_severity.Rmd",package = "episoap")
  # rmarkdown::render("/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/inst/rmarkdown/templates/test_severity.Rmd",
  #                   params = list(
  #                     DISEASE_NAME       = disease_name,
  #                     DATA               = data,
  #                     ACCOUNT_FOR_DELAY  = account_for_delay,
  #                     EPIDIST            = epidist
  #                   ),
  #                   output_dir = getwd(),
  #                   output_file = "test.html",
  #                   output_format = NULL)
  rmarkdown::render("/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/inst/rmarkdown/templates/test_severity.Rmd",
                    params        = parameters,
                    output_dir    = getwd(),
                    output_file   = "test.html",
                    output_format = NULL)
}

# cavits:
# printing function only show 1 result if it is a list
# need the defaults arguments when estimating cfr from count data

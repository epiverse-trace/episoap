#' Extract the transmissibility parameters from the input arguments
#'
#' @param parameters a list of parameters to be populated with the severity
#'    parameters
#' @param transmissibility_params a list with the user-specified arguments to be
#'    used for the CFR calculation
#'
#' @return the input list of parameters with extra arguments from the
#'    'transmissibility_params' object
#' @keywords internal
#'
get_transmissibility_params <- function(parameters, transmissibility_params) {
  if (!is.null(transmissibility_params)) {
    parameters[["INTERVAL"]] <- transmissibility_params[["interval"]]
    parameters[["USE_EPIPARAMETER_DATABASE"]] <- transmissibility_params[["use_epiparameter_database"]] # nolint: line_length_linter
    parameters[["GROUP_BY"]] <- transmissibility_params[["group_by"]]
    parameters[["SI"]]       <- transmissibility_params[["si"]]
    parameters[["SI_MEAN"]]  <- transmissibility_params[["si_mean"]]
    parameters[["SI_SD"]]    <- transmissibility_params[["si_sd"]]
    parameters[["SI_DIST"]]  <- transmissibility_params[["si_dist"]]
  }
  return(parameters)
}


#' Estimate the disease transmissibility
#'
#' Here the transmissibility is expressed as the reproduction number i.e the
#' number of people infected, on average, by a single infected person.
#'
#' @param data The input data frame or linelist
#' @param interval The size of the time interval used for computing the
#'    incidence
#' @param disease_name A string that represents the disease name
#' @param use_epiparameter_database A Boolean used to specify whether to extract
#'    the serial interval distribution from the {epiparameter} package or no.
#'    Default is `TRUE`.
#' @param group_by A string with the name of the variable by which the data will
#'    be grouped
#' @param si_mean A numeric representing the mean of the distribution for serial
#'    interval. This is only used when `use_epiparameter_database = FALSE`.
#' @param si_sd A numeric representing the standard deviation of the
#'    distribution for serial interval. This is only used when
#'    `use_epiparameter_database = FALSE`.
#' @param si_dist A string with the name of the probability distribution for
#'    the serial interval. This is only used when
#'    `use_epiparameter_database = FALSE`.
#' @param method A string with the name of the package to be used for the
#'    calculation of the reproduction ratio. Default is `i2extras`.
#'
#' @return a list with both descriptive statistics results, the growth rate, and
#'    the reproduction number.
#' @export
#'
#' @examples
get_transmissibility <- function(data,
                                 disease_name,
                                 date_var                  = "date",
                                 count_var                 = "n",
                                 group_var                 = "region",
                                 incomplete_days           = 7L,
                                 use_epiparameter_database = TRUE,
                                 si_mean                   = NULL,
                                 si_sd                     = NULL,
                                 si_dist                   = NULL) {

  # make sure that the date values are of class Date
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(\(x) inherits(x, "POSIXct")),
                                as.Date))

  # generate descriptive stats
  if (!inherits(data, "linelist")) {
    # convert the input data into linelist
    linelist_data <- data %>%
      linelist::make_linelist(date_admission = date_var,
                              location       = group_var,
                              counts         = count_var,
                              allow_extra    = TRUE)
    total_cases   <- plot_cases_per_group(linelist_data)

    # convert incidence into weekly incidence using incidence2
    converted_data <- data %>%
      incidence2::incidence(date_index = "date",
                            interval   = "week",
                            counts     = count_var,
                            groups     = group_var)

    small_counts      <- max(incidence2::get_count_value(converted_data)) < 20L
    weekly_incidence_plot <- plot_weekly_incidence(converted_data)
  }

  # generate the daily incidence data
  daily_incidence <- data |>
    incidence2::incidence(date_index = date_var,
                          interval   = "daily",
                          counts     = count_var,
                          groups     = group_var) %>%
    incidence2::keep_first(dplyr::n_distinct(.[["date_index"]]) -
                             incomplete_days)

  # calculate the growth rate using {i2extra}
  # Here we only consider a 'poisson' model (this can result in overestimating
  # the growth rate in presence of superspreading).
  growth_rate  <- estimate_growth_rate(daily_incidence,
                                       group_var = group_var,
                                       alpha     = alpha)

  # calculate the serial interval
  si_epidist <- get_si_distribution(
    disease_name              = disease_name,
    use_epiparameter_database = use_epiparameter_database,
    si_mean                   = si_mean,
    si_sd                     = si_sd,
    si_dist                   = si_dist
  )
  si         <- epiparameter::discretise(si_epidist)

  # make {EpiEstim} config
  ee_config  <- EpiEstim::make_config(si_distr = wrap_si(si))

  # calculate the global reproduction number over time
  res_epiestim_global <- daily_incidence |>
    incidence2::regroup() |>
    incidence2::get_count_value() |>
    EpiEstim::estimate_R(config = ee_config) |>
    wrap_res(daily_incidence)

  # global Rt plot
  global_Rt_plot <- plot_Rt(res_epiestim_global, group_var = NULL)

  # global Rt table
  global_Rt_table <- res_epiestim_global |>
    dplyr::mutate(
      date     = end,
      mean     = round(mean, 2L),
      median   = round(median, 2L),
      `95% ci` = sprintf(
        "[%1.2f ; %1.2f]",
        lower,
        upper
      )
    ) |>
    dplyr::select(date, mean, median, `95% ci`) |>
    dplyr::rename(
      "mean $R$"   = mean,
      "median $R$" = median
    )

  # calculate the per-group Rt
  res_epiestim_group <- daily_incidence |>
    tidyr::nest(data = c(incidence2::get_date_index_name(.),
                         incidence2::get_count_value_name(.))) |>
    dplyr::mutate(
      res_epiestim = purrr::map(data, ~ wrap_res(
        EpiEstim::estimate_R(.x[["count"]], config = ee_config),
        daily_incidence)
      )
    ) |>
    tidyr::unnest(res_epiestim) |>
    dplyr::select(-data)

  # per group Rt plot
  group_Rt_plot <- plot_Rt(res_epiestim_group, group_var = group_var)

  # per group Rt table
  group_Rt_table <- res_epiestim_group |>
    dplyr::filter(end == max(end)) |>
    dplyr::mutate(
      mean     = round(mean, 2L),
      median   = round(median, 2L),
      `95% ci` = sprintf(
        "[%1.2f ; %1.2f]",
        lower,
        upper
      )
    ) |>
    dplyr::select(-c(sd, lower, upper)) |>
    dplyr::rename(
      "mean $R$"   = mean,
      "median $R$" = median
    )

  return(list(
    growth_rate     = growth_rate,
    global_Rt_plot  = global_Rt_plot,
    global_Rt_table = global_Rt_table,
    group_Rt_plot   = group_Rt_plot,
    group_Rt_table  = group_Rt_table
  ))
}

#' Plot timely reproduction number
#'
#' @param plot_data A data frame with either the global or per group
#'    reproduction number.
#' @param group_var A string with the name of the column used to group the data.
#'    This is set to `NULL` when dealing with global reproduction number.
#'
#' @return An object of class <ggplot> that contains the plot data.
#' @keywords internal
#'
#' @examples
plot_Rt <- function(plot_data, group_var = NULL) {
  # Graph of all values over time
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = end)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                         fill = "#B2D1CC") +
    ggplot2::geom_line(ggplot2::aes(y = median), color = "#005C5D") +
    ggplot2::geom_hline(yintercept = 1, color = "#B45D75") +
    ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                              hjust = 1)) +
    ggplot2::labs(
      x = "",
      y = "Instantaneous reproduction number (Rt)",
      title = "Estimates of Rt (EpiEstim)"
    )

  if (!is.null(group_var)) {
    p <- p + ggplot2::facet_wrap(~ .data[[group_var]], ncol = 2L)
  }
  return(p)
}

#' Process output from `EpiEstim::estimate_R()` and output a tibble with dates,
#' mean R, and associated confidence intervals.
#'
#' @param x An `estimate_R` object
#' @param incid (Optional) The incidence2 object on which R values have been
#'   estimated.
#' @return An object of class <R_estimate> and <tibble>.
#' @keywords internal
#'
wrap_res <- function(x, incid = NULL) {
  stopifnot(inherits(x, "estimate_R"))
  out <- tibble::tibble(x[["R"]])
  out <- dplyr::select(out,
                       start  = t_start,
                       end    = t_end,
                       mean   = `Mean(R)`,
                       sd     = `Std(R)`,
                       median = `Median(R)`,
                       lower  = `Quantile.0.025(R)`,
                       upper  = `Quantile.0.975(R)`
  )
  if (!is.null(incid)) {
    stopifnot(inherits(incid, "incidence2"))
    dates          <- unique(incidence2::get_dates(incid))
    out[["start"]] <- dates[out[["start"]]]
    out[["end"]]   <- dates[out[["end"]]]
  }

  class(out) <- c("R_estimate", class(out))
  out
}

#' Process a distrcrete object and output `si_discr` argument for
#' EpiEstim::make_config
#'
#' @param si An object of class <epidist> with the serial interval information
#'
#' @return A numeric vector of the serial interval qvalues
#' @keywords internal
#'
wrap_si <- function(si) {
  domain <- seq(1L, to = si$prob_dist$qf(0.999), by = 1L)
  pmf    <- si$prob_dist$d(domain)
  pmf[1] <- 0
  pmf    <- pmf / sum(pmf)
  pmf
}

#' Get the serial interval distribution
#'
#' The serial interval distribution can be either generated using the
#' {epiparameter} package (if the disease is covered by the package) or the
#' user-provided parameters.
#'
#' @param disease_name A string with the disease name.
#' @param use_epiparameter_database A logical that specifies whether to use the
#'    {epiparameter} package or not.
#' @param si_mean A numeric that represents the mean serial interval value
#' @param si_sd A numeric that represents the standard deviation of the serial
#'    interval.
#' @param si_dist A string used to specify the name of the serial interval
#'    distribution. Use `?epiparameter::epidist` for more details about the
#'    possible values.
#'
#' @return An object of class <epidist> that contains the serial interval
#'    distribution
#' @keywords internal
#'
get_si_distribution <- function(disease_name,
                                use_epiparameter_database = TRUE,
                                si_mean                   = NULL,
                                si_sd                     = NULL,
                                si_dist                   = NULL) {
  # use {epiparameter} to get the serial interval distribution
  # is_parameterised <- TRUE
  if (use_epiparameter_database) {
    si_epidist <- epiparameter::epidist_db(
      disease        = disease_name,
      epi_dist       = "serial_interval",
      single_epidist = TRUE,
      subset         = is_parameterised
    )
    return(si_epidist)
  }

  # use user-provided parameters to get the serial interval distribution
  params_verification <- !is.null(si_mean) & !is.null(si_sd) & !is.null(si_dist)
  stopifnot("'si_mean', 'si_sd', 'si_dist' are require to estimate the serial
            interval when {epiparameter} is not used" = params_verification)
  summary_stats <- epiparameter::create_epidist_summary_stats(
    mean = si_mean,
    sd   = si_sd
  )
  si_epidist <- epiparameter::epidist(
    disease           = disease_name,
    epi_dist          = "serial_interval",
    prob_distribution = si_dist,
    summary_stats     = summary_stats,
    auto_calc_params  = TRUE
  )
  return(si_epidist)
}

#' Estimate the growth rate using {i2extra}
#'
#' @param dat A data frame with the daily incidence data
#' @param group_var A string with the name of the column used to group the data.
#'    This is `NULL` when the data is not grouped
#' @param alpha A numeric that represents the confidence interval level. Default
#'    is 95% (0.95)
#'
#' @return A data frame with the estimated growth rate
#' @keywords internal
#'
estimate_growth_rate <- function(dat, group_var = NULL, alpha = 0.95) {
  # estimate the growth rate
  out <- i2extras::fit_curve(dat,
                             model = "poisson",
                             alpha = alpha)
  gr  <- i2extras::growth_rate(out) |>
    dplyr::select(-c(1L, 3L)) |>
    dplyr::rename("period" = "growth_or_decay") |>
    dplyr::mutate("period type" = paste(period, "time"),
                  r = paste(round(r * 100, 1), "%"),
                  r_ci = sprintf("[%1.1f %% ; %1.1f %% ]",
                                 r_lower * 100,
                                 r_upper * 100),
                  time = round(time, 1),
                  time_ci = sprintf("[%.1f ; %.1f]", time_lower, time_upper)) |>
    dplyr::select("Daily growth rate" = r,
                  "Growth rate (95% CI)" = r_ci,
                  "period type",
                  "Duration (days)" = time,
                  "Duration (95% CI)" = time_ci)
  if (!is.null(group_var)) {
    gr            <- cbind(out[[group_var]], gr)
    names(gr)[1L] <- group_var
  }

  return(gr)
}

#' Plot the weekly incidence data
#'
#' @param converted_data An object of class <incidence2> with the weekly
#'    incidence data.
#'
#' @return An object of class <ggplot>
#' @keywords internal
#'
plot_weekly_incidence <- function(converted_data) {
  # general variables for automatic customisation of plots
  n_groups    <- dplyr::n_distinct(incidence2::get_groups(converted_data)[[1L]])
  weekly_incidence_plot <- converted_data %>%
    plot(alpha = 1, nrow = n_groups) +
    ggplot2::labs(title = "Weekly incidence of cases over time")
  return(weekly_incidence_plot)
}

#' Plot daily incidence data
#'
#' @param linelist_data A linelist with the raw count of the cases data
#'
#' @return An object of class <ggplot>
#' @keywords internal
#'
plot_cases_per_group <- function(linelist_data) {
  total_cases <- linelist_data %>%
    linelist::tags_df() %>%
    dplyr::select(location, counts) %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(cases = sum(counts)) %>%
    dplyr::mutate(location = forcats::fct_reorder(
      .f = location,
      .x = cases
    ))

  total_case_plot <- ggplot2::ggplot(total_cases, ggplot2::aes(x = cases,
                                                               y = location)) +
    ggplot2::geom_col(fill = "#5E7E80") +
    ggplot2::labs(x = "Total number of cases", y = NULL)

  total_case_table <- total_cases %>%
    dplyr::mutate(percentage = sprintf("%.2f%%", cases / sum(cases) * 100)) %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(cases = format(cases, scientific = FALSE, big.mark = " ")) %>%
    rlang::set_names(toupper)

  return(list(
    total_case_plot  = total_case_plot,
    total_case_table = total_case_table
  ))
}


#' Plot the serial interval distribution
#'
#' @param si An object of class <epidist> with the serial interval data
#' @param si_dist A string with the serial interval distribution name
#' @param si_mean A numeric with the mean serial interval
#' @param si_sd A numeric with the standard deviation of the serial interval
#'
#' @return An object of class <ggplot>
#' @keywords internal
#'
plot_si_distribution <- function(si, si_dist, si_mean, si_sd) {
  si_x <- seq(1L, to = si$q(0.999), by = 1L)
  dat  <- data.frame(delay = si_x, prob = si$d(si_x))
  si_distribution <- ggplot2::ggplot(dat, ggplot2::aes(x = delay, y = prob)) +
    ggplot2::geom_col(fill = "#5E7E80") +
    ggplot2::labs(
      title    = "Serial interval distribution",
      x        = "Days from primary to secondary onset",
      y        = "Probability",
      subtitle = sprintf("%s distribution | mean: %.1f days ; sd: %.1f days",
                         si_dist, si_mean, si_sd)
    )
  return(si_distribution)
}


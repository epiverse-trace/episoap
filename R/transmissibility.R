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
    are_incidence_params_in <- all(c("date_variable_name", "cases_status",
                                   "death_outcome", "diagnosis_status",
                                   "diagnosis_outcome") %in% names(transmissibility_params)) # nolint: line_length_linter

    # get parameters needed to create the incidence data
    if (!all(c("DATE_VARIABLE_NAME", "CASES_STATUS", "DEATH_OUTCOME",
              "DIAGNOSIS_STATUS", "DIAGNOSIS_OUTCOME") %in% names(parameters))) { # nolint: line_length_linter
      if (are_incidence_params_in) {
        parameters[["DATE_VARIABLE_NAME"]] <- transmissibility_params[["date_variable_name"]] # nolint: line_length_linter
        parameters[["CASES_STATUS"]]       <- transmissibility_params[["cases_status"]] # nolint: line_length_linter
        parameters[["DEATH_OUTCOME"]]      <- transmissibility_params[["death_outcome"]] # nolint: line_length_linter
        parameters[["DIAGNOSIS_STATUS"]]   <- transmissibility_params[["diagnosis_status"]] # nolint: line_length_linter
        parameters[["DIAGNOSIS_OUTCOME"]]  <- transmissibility_params[["diagnosis_outcome"]] # nolint: line_length_linter
      }
    }
  }
  return(parameters)
}


get_transmissibility <- function(data,
                                 group_by      = NULL,
                                 interval,
                                 si            = NULL,
                                 pathogen_name = NULL,
                                 si_mean       = NULL,
                                 si_sd         = NULL,
                                 si_dist       = NULL) {

  if (all(c("date", "cases") %in% names(data))) {
    # convert into daily incidence data
    if (!(unique(diff(data[["date"]])) == 1L)) {
      data  <- get_sequential_dates(data)
    }
  }

  # make sure that the date values are of class Date

  # convert to linelist
  if (!inherits(data, "linelist")) {
    date_var  <- "date"
    group_var <- group_by
    count_var <- "cases"
    linelist_data <- data %>%
      linelist::make_linelist(date_admission = date_var,
                              location       = group_var,
                              counts         = count_var,
                              allow_extra    = TRUE)
  }

  # convert incidence into chosen unit using incidence2
  converted_data <- data %>%
    incidence2::incidence("date",
                          interval = interval,
                          counts   = count_var,
                          groups   = group_var)

  small_counts         <- max(incidence2::get_count_value(converted_data)) < 20L
  weekly_incidence_plot <- plot_weekly_incidence(converted_data)
  total_cases           <- plot_cases_per_group(linelist_data)

  # next calculate R0
}

plot_weekly_incidence <- function(converted_data) {
  # general variables for automatic customisation of plots
  n_groups    <- dplyr::n_distinct(incidence2::get_groups(converted_data)[[1L]])
  weekly_incidence_plot <- converted_data %>%
    plot(alpha = 1, nrow = n_groups) +
    ggplot2::labs(title = "Weekly incidence of cases over time")
  return(weekly_incidence_plot)
}

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

  # use the below in the markdown to display the table
  # kbl() %>%
  #   kable_paper("striped", font_size = 18, full_width = FALSE)
  return(list(
    total_case_plot  = total_case_plot,
    total_case_table = total_case_table
  ))
}

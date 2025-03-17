

#' Determine input data type for the severity pipeline
#'
#' @param data A dataframe-like object that can be either data.frame, linelist or incidence. Default is NULL.
#' @param total_count A numeric with the total number of cases. Default is NULL.
#' @param total_death A numeric with the total number of deaths. Default is NULL.
#'
#' @returns  A character with one of the following values: "count_data", "linelist" or "incidence".
#' @export
#'
#' @examples
#'
#'
#'
get_data_type <- function( data = NULL, total_count = NULL, total_death = NULL ){

  checkmate::assert(
      check_data_frame(data, null.ok = TRUE),
      combine = "or"
    )


  # Check for count data
  if ( !is.null(total_count) && !is.null(total_death)){
    return("count_data")
  }

  # 2. Check for incidence objects
  if (inherits(data, "incidence")) {
    return("incidence")
  }

  if (inherits(data, "data.frame")) {
    # Convert column names to lowercase for consistent checks
    actual_cols <- tolower(names(data))
    required_incidence <- c("date", "cases", "dead") # atleast
    # linelist features
    linelist_keywords <- c("id", "case", "date", "onset", "report",
                           "age", "sex", "gender", "outcome", "symptom", "hospital")


    # 3a. Incident data check
    if (all(required_incidence %in% actual_cols)) {
      # Check if pure incident or has extras
      if (length(actual_cols) == 3) {
        return("incidence")
      } else {
        # Check extra columns for linelist features
        extra_cols <- actual_cols[!actual_cols %in% required_incidence]
        has_linelist <- any(extra_cols %in% linelist_keywords)
        return(ifelse(has_linelist, "linelist", "incidence"))
      }
    }


    # Count matches in original column names (case-insensitive)
    col_matches <- grepl(paste(linelist_keywords, collapse = "|"),
                         names(data), ignore.case = TRUE)

    if (sum(col_matches) >= 4) {
      return("linelist")
    }

  }

  # Default/error case
  stop("unknown_data_type Provide count args, linelist, or incidence.")
}










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
#' # 1. COUNT DATA EXAMPLE
#' # When totals are provided directly
#' get_data_type(total_count = 1500, total_death = 75)  # Returns "count_data"
#'
#' # 2. INCIDENCE OBJECT EXAMPLE
#' # When using the incidence package's format
#' dummy_inc <- structure(list(date = Sys.Date(), cases = 100, dead = 5),
#'                        class = "incidence")
#' get_data_type(data = dummy_inc)  # Returns "incidence"
#'
#' # 3. LINELIST EXAMPLE
#' # Detailed case-based data with key identifiers
#' linelist_df <- data.frame(
#'   ID = 1:100,
#'   ONSET_DATE = Sys.Date() - 1:100,
#'   REPORT_DATE = Sys.Date(),
#'   AGE = sample(5:80, 100, replace = TRUE),
#'   OUTCOME = sample(c("Fatal", "Recovered"), 100, replace = TRUE)
#' )
#' get_data_type(data = linelist_df)  # Returns "linelist"
#'
#' # 4. SIMPLE INCIDENCE DATA FRAME
#' # Minimal time-based case counts
#' inc_df <- data.frame(
#'   Date = seq.Date(Sys.Date(), by = "day", length.out = 10),
#'   Cases = sample(10:50, 10),
#'   Dead = sample(0:5, 10)
#' )
#' get_data_type(data = inc_df)  # Returns "incidence"
#'
#' # 5. ERROR CASE
#' # Missing both data and counts
#' try(get_data_type())  # Throws  error message

get_data_type <- function( data = NULL, total_count = NULL, total_death = NULL ){

  # validate inputs with checkmate
  checkmate::assert(
      checkmate::check_data_frame(data, null.ok = TRUE),
      checkmate::check_class(data, classes = c("linelist", "incidence"), null.ok = TRUE),
      combine = "or"
    )

  # Check if 'total_count'  and total_death' are a single numeric value or NULL
  checkmate::assert_number(total_count, null.ok = TRUE)
  checkmate::assert_number(total_death, null.ok = TRUE)


# check  'total_count' and 'total_death' are non-negative
  if (!is.null(total_count)) checkmate::assert_number(total_count, lower = 0)
  if (!is.null(total_death)) checkmate::assert_number(total_death, lower = 0)

  # Check for count data
  if ( !is.null(total_count) && !is.null(total_death)) {
    return("count_data")
  }


  #  Check for incidence objects
  if (inherits(data, "incidence")) {
    return("incidence")
  }

  # check for data.frame/linelist
  if (inherits(data, "data.frame")) {
    # Convert column names to lowercase for consistent checks
    actual_cols <- tolower(names(data))
    required_incidence <- c("date", "cases", "dead") # atleast
    # linelist features
    linelist_keywords <- c("id", "case", "date", "onset", "report",
                           "age", "sex", "gender", "outcome", "symptom", "hospital")


    #  Incident data check
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
  stop("unknown_data_type! Either provide a non-negative value for  total_count and total_death arguements or  a dataframe-like object (data.frame, linelist or incidence),in the data arguement")
}








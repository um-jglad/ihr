#' Calculate the % missingness of the heart rate value

#' @description
#' The function percent_nonmissing computes the percentage missingness of the heart rate data.

#' @usage
#' percent_nonmissing(data)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of percentage missingness is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' percent_nonmissing(example_heart_1)

percent_nonmissing <- function(data, tz = "") {
  time = hr = id = time_date = time_min = unique_days = total_days = NULL
  rm(list = c('time', 'hr', 'id', 'time_date', 'unique_days', 'total_days', 'time_min'))
  # Ensure necessary columns exist
  if (!all(c("id", "time") %in% colnames(data))) {
    stop("The dataset must contain 'id' and 'time' columns.")
  }

  # Convert time column to Date-Time format
  data <- data |>
    dplyr::mutate(time = as.POSIXct(time, format = "%m/%d/%Y %I:%M:%S", tz = tz),
                  time_date = as.Date(time),
                  time_min = as.POSIXct(strftime(time, "%Y-%m-%d %H:%M:00"), tz = tz
                  ))  # Extract only the date

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(hr))

  # Group by id and compute active percentage and measurement period length
  active_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarize(
      start_time = min(time),
      end_time = max(time),
      unique_days = length(unique(format(time, "%m-%d-%Y"))),
      total_days = as.integer(difftime(max(time_date), min(time_date), units = "days")) + 1,
      percent_nonmissing = dplyr::n_distinct(time_min) / (as.integer(difftime(max(time_min), min(time_min),
                                                                              units = "mins")) + 1) * 100
    )

  return(active_data)
}

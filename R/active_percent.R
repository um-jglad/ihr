#' Calculate the % missingness of the heart rate value

#' @Description
#' The function active_percent computes the percentage missingness of the heart rate data.

#' @Usage
#' active_percent(data)

#' @para
#' data: A DataFrame object with column names "Id", "Time", "Value".
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of percentage missingness is returned.
#' 'NA' heartrate values are omitted from the calcution of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' active_percent(example_heart_1)

active_percent <- function(data) {
  time = hr = id = time_date = unique_days = total_days = NULL
  rm(list = c('time', 'hr', 'id', 'time_date', 'unique_days', 'total_days'))
  # Ensure necessary columns exist
  if (!all(c("id", "time") %in% colnames(data))) {
    stop("The dataset must contain 'id' and 'time' columns.")
  }

  # Convert time column to Date-Time format
  data <- data |>
    dplyr::mutate(time = as.POSIXct(time, format = "%m/%d/%Y %I:%M:%S %p"),
           time_date = as.Date(time))  # Extract only the date

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(hr))

  # Group by id and compute active percentage and measurement period length
  active_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarize(
      total_days = as.integer(difftime(max(time), min(time), units = "days")) + 1,
      unique_days = dplyr::n_distinct(time_date),
      active_percent = (unique_days / total_days) * 100
    )

  return(active_data)
}

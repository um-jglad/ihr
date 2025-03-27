#' Calculate the % missingness of the heart rate value

#' @description
#' The function percent_nonmissing computes the percentage missingness of the heart rate data.

#' @usage
#' percent_nonmissing(data)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of percentage missingness is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' percent_nonmissing(example_heart_1)

percent_nonmissing <- function(data) {
  time = hr = id = time_date = unique_days = total_days = NULL
  rm(list = c('time', 'hr', 'id', 'time_date', 'unique_days', 'total_days'))
  # Ensure necessary columns exist
  if (!all(c("id", "time") %in% colnames(data))) {
    stop("The dataset must contain 'id' and 'time' columns.")
  }

  # Convert time column to Date-Time format
  data <- data |>
    dplyr::mutate(time = as.POSIXct(time, format = "%m/%d/%Y %I:%M:%S"),
                  time_date = as.Date(time))  # Extract only the date

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(hr))

  # Group by id and compute active percentage and measurement period length
  active_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarize(
      start_date = min(time),
      end_date = max(time),
      unique_days = length(unique(format(time, "%m-%d-%Y"))),
      total_days = as.integer(difftime(max(time_date), min(time_date), units = "days")) + 1,
      active_percent = (unique_days / total_days) * 100
    )

  return(active_data)
}

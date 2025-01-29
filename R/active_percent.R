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
  # Ensure necessary columns exist
  if (!all(c("Id", "Time") %in% colnames(data))) {
    stop("The dataset must contain 'Id' and 'Time' columns.")
  }

  # Convert Time column to Date-Time format
  data <- data %>%
    mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p"),
           Time_minute = floor_date(Time, "minute"))

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(Value))

  # Group by Id and compute active percentage and measurement period length
  active_data <- data %>%
    group_by(Id) %>%
    summarize(
      total_minutes = as.numeric(difftime(max(Time), min(Time), units = "mins")),
      unique_minutes = n_distinct(Time_minute),
      active_percent = (unique_minutes / total_minutes) * 100
    )

  return(active_data)
}

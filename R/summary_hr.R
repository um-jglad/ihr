#' Calculate summary heart rate

#' @Description
#' The function summary_hr computes summary statistics for heart rate (HR) data.
#' It is a wrapper for summary() functions and outputs a tibble object with the subject ID and the following summary values:
#' Minimum, 1st Quartile, Median, Mean, 3rd Quartile, Maximum, Standard Deviation (SD), Total number of unique days.

#' @Usage
#' summary_hr(data)

#' @para
#' data: A DataFrame object with column names "Id", "Time", "Value".
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calcution of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' summary_hr(example_heart_1)

summary_hr <- function(data) {
  # Ensure "Id" and "HR Value" are present
  if (!all(c("Id", "Value") %in% colnames(data))) {
    stop("The dataset must contain 'Id' and 'Value' columns.")
  }

  # Convert Time column to Date-Time format
  data <- data %>%
    mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p"))

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(Value))

  # Group by ID and compute summary statistics for HR
  summary_data <- data %>%
    dplyr::group_by(Id) %>%
    dplyr::summarize(
      mean_hr = mean(Value, na.rm = TRUE),
      median_hr = median(Value, na.rm = TRUE),
      min_hr = min(Value, na.rm = TRUE),
      q1_hr = quantile(Value, 0.25, na.rm = TRUE),
      q3_hr = quantile(Value, 0.75, na.rm = TRUE),
      max_hr = max(Value, na.rm = TRUE),
      sd_hr = sd(Value, na.rm = TRUE),
      total_days = length(unique(format(Time, "%Y-%m-%d")))
    )

  return(summary_data)
}

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
  if (!all(c("id", "hr") %in% colnames(data))) {
    stop("The dataset must contain 'id' and 'hr' columns.")
  }

  # Convert Time column to Date-Time format
  data <- data %>%
    mutate(time = as.POSIXct(time, format = "%m/%d/%Y %I:%M:%S %p"))

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(hr))

  # Group by ID and compute summary statistics for HR
  summary_data <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(
      mean_hr = mean(hr, na.rm = TRUE),
      median_hr = median(hr, na.rm = TRUE),
      min_hr = min(hr, na.rm = TRUE),
      q1_hr = quantile(hr, 0.25, na.rm = TRUE),
      q3_hr = quantile(hr, 0.75, na.rm = TRUE),
      max_hr = max(hr, na.rm = TRUE),
      sd_hr = sd(hr, na.rm = TRUE),
      total_days = length(unique(format(time, "%Y-%m-%d")))
    )

  return(summary_data)
}

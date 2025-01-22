#' Calculate summary heart rate

#' @Description
#' The function summary_hr computes summary statistics for heart rate (HR) data. 
#' It is a wrapper for summary() functions and outputs a tibble object with the subject ID and the following summary values:
#' Minimum, 1st Quartile, Median, Mean, 3rd Quartile, Maximum, Standard Deviation (SD), Total number of unique days, and Total number of HR observations (minutes).

#' @Usage
#' summary_hr(data)

#' @para
#' data: A DataFrame object with column names "ID", "Day_Count", "Day", "Minute", and "HR". 
#' Missing HR values (NA) are automatically excluded from calculations.

summary_hr <- function(data) {
  # Ensure "ID" and "HR" are present
  if (!all(c("ID", "HR") %in% colnames(data))) {
    stop("The dataset must contain 'ID' and 'HR' columns.")
  }
  
  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(HR))
  
  # Group by ID and compute summary statistics for HR
  summary_data <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::summarize(
      mean_hr = mean(HR, na.rm = TRUE),
      median_hr = median(HR, na.rm = TRUE),
      min_hr = min(HR, na.rm = TRUE),
      max_hr = max(HR, na.rm = TRUE),
      total_minutes = n()
    )
  
  return(summary_data)
}

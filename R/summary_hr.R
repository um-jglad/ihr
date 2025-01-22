

summary_hr <- function(data) {
  # Ensure necessary columns are present
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
      count = n()
    )
  
  return(summary_data)
}

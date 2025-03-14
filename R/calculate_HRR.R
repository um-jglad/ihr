calculate_HRR <- function(data) {
  time = hr = id= NULL
  rm(list = c('time', 'hr', 'id'))
  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")

  RHR_data <- calculate_RHR(data)
  
  # If no RHR data, return NULL
  if (is.null(RHR_data)) {
    print("HRR cannot be calculated because RHR is missing.")
    return(NULL)
  }

  HRR_hr_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarize(max_hr = max(hr, na.rm = TRUE), 
                     min_hr = min(hr, na.rm = TRUE),
                     .groups = 'drop')
  
  HRR_hr_data <- dplyr::left_join(HRR_hr_data, RHR_data, by = "id")

  HRR_hr_data <- HRR_hr_data |>
    dplyr::mutate(HRR = max_hr - RHR)

  return(HRR_hr_data)
}

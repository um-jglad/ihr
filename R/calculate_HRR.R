#' Calculate heart rate reserve

#' @description
#' The function calculate_RHR computes the heart rate reserve.
#' It calculates HRR using the formula:maximum heart rate - resting heart rate following the definition gained from the reference.

#' @usage
#' calculate_HRR(data)

#' @param data A DataFrame object with column names "id", "time", "hr".

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Heart rate reserve as a predictor of cardiovascular and all-cause mortality in men,
#' \doi{10.1249/01.MSS.0000038959.67619.30}.

#' @export
#' @examples
#' data(example_heart_1)
#' calculate_HRR(example_heart_1)

calculate_HRR <- function(data) {
  time = hr = id = max_hr = min_hr = RHR = NULL
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

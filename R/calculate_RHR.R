#' Calculate resting heart rate

#' @description
#' The function calculate_RHR computes the resting heart rate.
#' It calculates the mean hr value between 3am to 7am and consider it as the resting heart rate according to the reference.

#' @usage
#' calculate_RHR(data)

#' @param data A DataFrame object with column names "id", "time", "hr".

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Measure by measure: Resting heart rate across the 24-hour cycle,
#' \doi{10.1371/journal.pdig.0000236}.

#' @export
#' @examples
#' data(example_heart_1)
#' summary_hr(example_heart_1)


calculate_RHR <- function(data) {
  time = hr = id = hour = NULL
  rm(list = c('time', 'hr', 'id', "hour"))
  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")
  data$hour <- format(data$time, "%H")
  filtered_data <- subset(data, hour >= "03" & hour < "07")
  mean_hr <- mean(filtered_data$hr, na.rm = TRUE)
  return(mean_hr)
}

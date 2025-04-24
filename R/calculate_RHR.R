#' Calculate resting heart rate

#' @description
#' The function calculate_RHR computes the resting heart rate.
#' It calculates the mean hr value between 3am to 7am and consider it as the resting heart rate according to the reference.

#' @usage
#' calculate_RHR(data, tz)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Measure by measure: Resting heart rate across the 24-hour cycle,
#' \doi{10.1371/journal.pdig.0000236}.

#' @export
#' @examples
#' data(example_heart_1)
#' calculate_RHR(example_heart_1)


calculate_RHR <- function(data, tz = "") {
  time = hr = id = hour = NULL
  rm(list = c('time', 'hr', 'id', 'hour'))
  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S", tz = tz)
  data$hour <- format(data$time, "%H")
  filtered_data <- subset(data, hour >= "03" & hour < "07")

  if (nrow(filtered_data) == 0) {
    print("No data between 03:00 and 07:00")
    return(NULL)  # Return NULL to indicate no data
  }

  rhr_data <- filtered_data |>
    dplyr::group_by(id) |>
    dplyr::summarize(RHR = mean(hr, na.rm = TRUE), .groups = 'drop')
  return(rhr_data)
}
